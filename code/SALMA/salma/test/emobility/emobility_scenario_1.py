import unittest
import random

from statsmodels.stats import proportion

from salma.model.distributions import NormalDistribution, \
    ConstantDistribution, Distribution
from salma.model.selectionstrategy import Stepwise

from salma.test.emobility.map_generator import MapGenerator
from salma.test.emobility.map_translator import MapTranslator
from salma.model.world import World
from salma.test.emobility.emobility_base import EMobilityBase
from salma.statistics import SequentialProbabilityRatioTest
from salma.test.emobility.vehicle import create_vehicles
from salma.test.emobility.plcs import create_plcs_processes
from salma.test.emobility.plcssam import create_plcssam
import numpy as np


HYPTEST, ESTIMATION, VISUALIZE = range(3)

_MODE = VISUALIZE


class RoadTravelTimeDistribution(Distribution):
    def __init__(self, relative_sd):
        super().__init__("integer", (float("-inf"), float("inf")))
        self.__relative_sd = relative_sd

    def generateSample(self, evaluationContext, paramValues):
        world = World.instance()
        road = evaluationContext.get_derived_fluent_value("currentRoad", paramValues[0])
        if road is not None:
            road_length = world.getConstantValue("roadlength", [road])
            speed = world.getConstantValue("roadBaseSpeed", [road])
            base_duration = road_length / speed
            d = random.normalvariate(base_duration, self.__relative_sd * base_duration)
            return round(d)
        else:
            return None


class EMobilityScenario1(EMobilityBase):
    NUM_OF_VEHICLES = 5
    PLCS_CAPACITY = 10
    VEHICLE_SPEED = 5
    TIME_LIMIT = 152

    def __print_info(self, world):
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
        problematic_stochastic_actions, problematic_exogenous_actions = world.check_action_initialization()
        print("-" * 80)
        print("Uninitialized stochastic actions:")
        print(problematic_stochastic_actions)
        print("-" * 80)
        print("Uninitialized exogenous actions:")
        print(problematic_exogenous_actions)
        world.printState()
        p1, p2 = world.check_action_initialization()
        print(p1)
        print(p2)

    def create_entities(self):

        super().create_entities()


    def test_scenario1(self):
        world = World.instance()
        log = False



        create_plcssam(world, world_map, mt)
        create_vehicles(world, world_map, mt, EMobilityScenario1.NUM_OF_VEHICLES)
        # load graph
        self.init_map_and_defaults(world, world_map, mt)

        create_plcs_processes(world, world_map, mt)

        vehicles = world.getAgents("vehicle")
        crossings = list(world.getDomain("crossing"))
        starts = crossings.copy()
        pois = list(world.getDomain("poi"))
        target_pois = pois.copy()
        sams = world.getAgents("plcssam")
        plcses = world.getAgents("plcs")

        # -------------------------------
        # Simulation config
        # -------------------------------
        for plcs in plcses:
            world.setConstantValue("maxCapacity", [plcs.id], EMobilityScenario1.PLCS_CAPACITY)
            world.setConstantValue("plcsChargeRate", [plcs.id], 0.05)
            plcs.initialize_connector_processes(default_sense_period=5, default_remote_sensor_send_period=5)

        for vehicle in vehicles:
            start = random.choice(starts)
            # starts.remove(start)
            target_poi = random.choice(target_pois)
            # target_pois.remove(target_poi)
            world.setFluentValue("vehiclePosition", [vehicle.id], ("l", start.id))
            world.setFluentValue("batteryLevel", [vehicle.id], 100.0)
            world.setConstantValue("baseDischargeRate", [vehicle.id], 0.01)
            world.setFluentValue("currentTargetPOI", [vehicle.id], target_poi.id)
            world.setConstantValue("calendar", [vehicle.id], [("cal", target_poi.id, 0, 0)])
            vehicle.initialize_connector_processes(default_sense_period=5, default_remote_sensor_send_period=5)
        for sam in sams:
            sam.initialize_connector_processes(default_sense_period=5, default_remote_sensor_send_period=5)

        transfer_starts = world.get_exogenous_action("transferStarts")
        transfer_starts.config.occurrence_distribution = NormalDistribution("float", 5, 1)
        transfer_starts.config.set_param_distribution("error", ConstantDistribution("term", None))

        transfer_ends = world.get_exogenous_action("transferEnds")
        transfer_ends.config.occurrence_distribution = NormalDistribution("float", 5, 1)
        transfer_ends.config.set_param_distribution("error", ConstantDistribution("term", None))

        transfer_fails = world.get_exogenous_action("transferFails")
        transfer_fails.config.occurrence_distribution = NormalDistribution("float", 5, 1)

        message_start = world.get_exogenous_action_choice("message_start")
        message_start.selection_strategy = Stepwise(transferStarts=0.9, transferFails=0.1)

        message_end = world.get_exogenous_action_choice("message_end")
        message_end.selection_strategy = Stepwise(transferEnds=0.8, transferFails=0.2)

        enterNextRoad = world.get_exogenous_action("enterNextRoad")
        enterNextRoad.config.occurrence_distribution = NormalDistribution("float", 5, 4)

        arriveAtRoadEnd = world.get_exogenous_action("arriveAtRoadEnd")
        arriveAtRoadEnd.config.occurrence_distribution = RoadTravelTimeDistribution(0.2)

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
        """.format(EMobilityScenario1.TIME_LIMIT)

        goal1 = """
        forall(v:vehicle,
            and(
                currentTargetPLCS(v) \= none,
                currentPLCS
            )
        )
        """
        goal2 = "forall(v:vehicle, currentTargetPLCS(v) \= none)"
        if _MODE == VISUALIZE:
            world.registerProperty("g", goal1, World.ACHIEVE)
        else:
            world.registerProperty("f", fstr, World.INVARIANT)
            world.registerProperty("g", goal2, World.ACHIEVE)

        if log:
            self.__print_info(world)

        # verdict, results = self.run_experiment(world, world_map, log=False, visualize=False)
        # print("Verdict: {}\nResults:\n{}".format(verdict, results))
        # results, infos = world.run_repetitions(100)

        # assumption success prob = 0.6 --> H0: p <= 0.4
        if _MODE == HYPTEST:
            sprt = SequentialProbabilityRatioTest(0.59, 0.61, 0.05, 0.05)
            accepted_hypothesis, results, info = world.run_repetitions(hypothesis_test=sprt)
            print("SPRT")
            print("Conducted tests: {}".format(len(results)))
            print("Successes: {} of {}".format(sum(results), len(results)))
            print("Hypothesis accepted: {}".format(accepted_hypothesis))

            self.print_timing_info(info)
        elif _MODE == ESTIMATION:
            print("len: ", len(world.getDomain("plcs")))
            _, results, info = world.run_repetitions(number_of_repetitions=20)

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

            self.print_timing_info(info)

        elif _MODE == VISUALIZE:
            print("Visualize")
            verdict, _ = self.run_experiment(world, world_map, log=True, visualize=True)
            print("Verdict: {}".format(verdict))

    def print_timing_info(self, info):
        steps = [ti["steps"] for ti in info]

        times = [ti["time"].total_seconds() for ti in info]
        print(
            "Steps: mean = {}, median = {}, min={}, max={}".format(np.mean(steps), np.median(steps), np.min(steps),
                                                                   np.max(steps)))
        print("Time: mean = {}, median = {}, min={}, max={}".format(np.mean(times), np.median(times), np.min(times),
                                                                    np.max(times)))


if __name__ == '__main__':
    unittest.main()
