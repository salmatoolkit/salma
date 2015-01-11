import random

from statsmodels.stats import proportion
from salma.constants import ACHIEVE, INVARIANT
from salma.model.data import Term

from salma.model.distributions import NormalDistribution, \
    ConstantDistribution, Distribution, OptionalDistribution, UniformDistribution
from salma.model.selectionstrategy import Stepwise

from salma.model.world import World
from salma.test.emobility.emobility_base import EMobilityBase, print_timing_info
from salma.statistics import SequentialProbabilityRatioTest
from salma.test.emobility.vehicle import create_vehicles
from salma.test.emobility.plcs import create_plcs_processes
from salma.test.emobility.plcssam import create_plcssam


HYPTEST, ESTIMATION, VISUALIZE = range(3)

_MODE = VISUALIZE

NUM_OF_VEHICLES = 5
PLCS_CAPACITY = 10
VEHICLE_SPEED = 5
TIME_LIMIT = 152


class RoadTravelTimeDistribution(Distribution):
    def __init__(self, relative_sd):
        super().__init__("integer")
        self.__relative_sd = relative_sd

    def generateSample(self, evaluation_context, param_values):
        world = World.instance()
        road = evaluation_context.get_derived_fluent_value("currentRoad", [param_values[0]])
        if road is not None:
            road_length = world.getConstantValue("roadlength", [road])
            assert isinstance(road_length, (float, int))
            speed = world.getConstantValue("roadBaseSpeed", [road])
            assert isinstance(speed, (float, int))
            base_duration = road_length / speed
            d = random.normalvariate(base_duration, self.__relative_sd * base_duration)
            return max(0, round(d))
        else:
            return None


class EMobilityScenario1(EMobilityBase):

    def __init__(self):
        super().__init__()

    @staticmethod
    def __print_info(world):
        """
        :param World world: the world
        """
        uninitialized_fluent_instances, uninitialized_constant_instances = world.check_fluent_initialization()
        print("-" * 80)
        print("Uninitialized Fluents:")
        print(uninitialized_fluent_instances)
        print("-" * 80)

        print("Uninitialized Constants:")
        print(uninitialized_constant_instances)
        problematic_stochastic_actions, problematic_exogenous_actions, problematic_action_choices = \
            world.check_action_initialization()
        print("-" * 80)
        print("Uninitialized stochastic actions:")
        print(problematic_stochastic_actions)
        print("-" * 80)
        print("Uninitialized exogenous actions:")
        print(problematic_exogenous_actions)
        print("-" * 80)
        print("Uninitialized exogenous action choices:")
        print(problematic_action_choices)
        world.printState()

    def create_entities(self):
        super().create_entities()
        create_plcssam(self.world, self.world, self.mt)
        create_vehicles(self.world, self.world_map, self.mt, NUM_OF_VEHICLES)
        create_plcs_processes(self.world, self.world_map, self.mt)

    def create_initial_situation(self):
        super().create_initial_situation()
        vehicles = self.world.getAgents("vehicle")
        crossings = list(self.world.getDomain("crossing"))
        starts = crossings.copy()
        pois = list(self.world.getDomain("poi"))
        target_pois = pois.copy()
        sams = self.world.getAgents("plcssam")
        plcses = self.world.getAgents("plcs")

        # -------------------------------
        # Simulation config
        # -------------------------------
        for plcs in plcses:
            self.world.setConstantValue("maxCapacity", [plcs.id], PLCS_CAPACITY)
            self.world.setConstantValue("plcsChargeRate", [plcs.id], 0.05)
            plcs.initialize_connector_processes(default_sense_period=5, default_remote_sensor_send_period=5)

        for vehicle in vehicles:
            start = random.choice(starts)
            # starts.remove(start)
            target_poi = random.choice(target_pois)
            # target_pois.remove(target_poi)
            self.world.setFluentValue("vehiclePosition", [vehicle.id], Term("l", start.id))
            self.world.setFluentValue("batteryLevel", [vehicle.id], 100.0)
            self.world.setConstantValue("baseDischargeRate", [vehicle.id], 0.01)
            self.world.setFluentValue("currentTargetPOI", [vehicle.id], target_poi.id)
            self.world.setConstantValue("calendar", [vehicle.id], [Term("cal", target_poi.id, 0, 0)])
            vehicle.initialize_connector_processes(default_sense_period=5, default_remote_sensor_send_period=5)
        for sam in sams:
            sam.initialize_connector_processes(default_sense_period=5, default_remote_sensor_send_period=5)

    def setup_distributions(self):
        super().setup_distributions()
        transfer_starts = self.world.get_exogenous_action("transferStarts")
        transfer_starts.config.occurrence_distribution = NormalDistribution("integer", 5, 1)
        transfer_starts.config.set_param_distribution("error", ConstantDistribution("term", None))

        transfer_ends = self.world.get_exogenous_action("transferEnds")
        transfer_ends.config.occurrence_distribution = NormalDistribution("integer", 5, 1)
        transfer_ends.config.set_param_distribution("error", ConstantDistribution("term", None))

        transfer_fails = self.world.get_exogenous_action("transferFails")
        transfer_fails.config.occurrence_distribution = NormalDistribution("integer", 5, 1)

        message_start = self.world.get_exogenous_action_choice("message_start")
        message_start.selection_strategy = Stepwise(transferStarts=0.9, transferFails=0.1)

        message_end = self.world.get_exogenous_action_choice("message_end")
        message_end.selection_strategy = Stepwise(transferEnds=0.8, transferFails=0.2)

        enterNextRoad = self.world.get_exogenous_action("enterNextRoad")
        enterNextRoad.config.occurrence_distribution = UniformDistribution("integer",
                                                                           value_range=(3, 4))

        arriveAtRoadEnd = self.world.get_exogenous_action("arriveAtRoadEnd")
        arriveAtRoadEnd.config.occurrence_distribution = RoadTravelTimeDistribution(0.2)

        driverParksAtPLCS = self.world.get_exogenous_action("driverParksAtPLCS")
        driverParksAtPLCS.config.occurrence_distribution = UniformDistribution("integer",
                                                                               value_range=(3, 4))

        driverLeavesPLCS = self.world.get_exogenous_action("driverLeavesPLCS")
        driverLeavesPLCS.config.occurrence_distribution = OptionalDistribution(
            0.0, UniformDistribution("integer",
                                     value_range=(3, 4)))

    def run_scenario1(self):
        log = True

        self.prepare(log, _MODE == VISUALIZE)
        fstr = """
        forall(v:vehicle,
            implies(
                messageSent(v, assignment, ?, ?, ?, ?),
                until({},
                    true,
                    currentTargetPLCS(v) \= none
                )
            )
        )
        """.format(TIME_LIMIT)

        goal1 = """
        forall(v:vehicle,
            let(myplcs : currentTargetPLCS(v),
                and(
                    myplcs \= none,
                    currentPLCS(v) = myplcs
                )
            )
        )
        """
        goal2 = "forall(v:vehicle, currentTargetPLCS(v) \= none)"
        if _MODE == VISUALIZE:
            self.property_collection.register_property("g", goal1, ACHIEVE)
        else:
            self.property_collection.register_property("f", fstr, INVARIANT)
            self.property_collection.register_property("g", goal2, ACHIEVE)

        if log:
            self.__print_info(self.world)

        # assumption success prob = 0.6 --> H0: p <= 0.4
        if _MODE == HYPTEST:
            sprt = SequentialProbabilityRatioTest(0.59, 0.61, 0.05, 0.05)
            accepted_hypothesis, results, info = self.run_repetitions(hypothesis_test=sprt,
                                                                      step_listeners=self.step_listeners)
            print("SPRT")
            print("Conducted tests: {}".format(len(results)))
            print("Successes: {} of {}".format(sum(results), len(results)))
            print("Hypothesis accepted: {}".format(accepted_hypothesis))

            print_timing_info(info)
        elif _MODE == ESTIMATION:
            print("len: ", len(self.world.getDomain("plcs")))
            _, results, info = self.run_repetitions(number_of_repetitions=20, step_listeners=self.step_listeners)

            successes = sum(results)
            nobs = len(results)
            ratio = successes / nobs
            print("ESTIMATION")
            print("Successes: {} of {}".format(successes, nobs))
            print("Ratio: {}".format(ratio))
            alpha = 0.05

            print("Confidence intervals, alpha={}: ".format(alpha))
            for method in ["normal", "agresti_coull", "beta", "wilson", "jeffrey"]:
                ci_low, ci_upp = proportion.proportion_confint(successes, nobs, alpha=alpha, method=method)
                print("   {} => [{}..{}]".format(method, ci_low, ci_upp))

            print_timing_info(info)

        elif _MODE == VISUALIZE:
            print("Visualize")
            verdict, info = self.run_experiment(step_listeners=self.step_listeners)

            print("Verdict: {}".format(verdict))
            print("Info: {}".format(info))


if __name__ == '__main__':
    sc1 = EMobilityScenario1()
    sc1.initialize()
    sc1.run_scenario1()
