import random

from salma import SALMAException
from salma.constants import ACHIEVE, INVARIANT
from salma.model.data import Term
from salma.model.distributions import NormalDistribution, \
    ConstantDistribution, Distribution, OptionalDistribution, UniformDistribution
from salma.model.selectionstrategy import Stepwise
from salma.model.world import World
from emobility_base import EMobilityBase
from vehicle import create_vehicles
from plcs import create_plcs_processes
from plcssam import create_plcssam


HYPTEST, ESTIMATION, VISUALIZE, LOG = range(4)


DEFAULT_NUM_OF_VEHICLES = 5
DEFAULT_PLCS_CAPACITY = 10
DEFAULT_VEHICLE_SPEED = 5
DEFAULT_TIME_LIMIT = 110


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


class MessageDelayDistribution(Distribution):
    def __init__(self, connector_specs, default=None):
        """
        :param dict[str, (int, int)] connector_specs: connector specifications
        """
        super().__init__("integer")
        self.__connector_specs = connector_specs
        self.__default = default

    def generateSample(self, evaluation_context, param_values):
        world = World.instance()
        mid = param_values[0]
        spec = world.getConstantValue("message_spec", [mid])
        assert isinstance(spec, Term)
        con, msg_type, agent, _ = spec.params
        if con not in self.__connector_specs:
            if self.__default is None:
                raise SALMAException("No parameters defined for message delay distribution of connector con!")
            else:
                mu, sigma = self.__default
        else:
            mu, sigma = self.__connector_specs[con]
        d = random.normalvariate(mu, sigma)
        return max(0, round(d))


class EMobilityScenario1(EMobilityBase):

    def __init__(self, mode, should_log, num_vehicles=DEFAULT_NUM_OF_VEHICLES,
                 plcs_capacity=DEFAULT_PLCS_CAPACITY,
                 vehicle_speed=DEFAULT_VEHICLE_SPEED,
                 time_limit=DEFAULT_TIME_LIMIT):
        """
        Creates a new instance of the E-Mobility scenario.

        :param int mode: one of VISUALIZE, ESTIMATION, or HYPTEST
        :param bool should_log: activates logging if True.
        :param int num_vehicles: the number of vehicles
        :param int plcs_capacity: the capacity of each PLCS
        :param int vehicle_speed: the average number of road length units that a vehicle moves per time unit
        :param int time_limit: the time limit for each vehicle to receive a target PLCS assignment after
            it has sent a request.
        """
        super().__init__(should_log, mode == VISUALIZE)
        self.__mode = mode
        self.__num_vehicles = num_vehicles
        self.__plcs_capacity = plcs_capacity
        self.__vehicle_speed = vehicle_speed
        self.__time_limit = time_limit

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
        create_vehicles(self.world, self.world_map, self.mt, self.__num_vehicles)
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
            self.world.setConstantValue("maxCapacity", [plcs.id], self.__plcs_capacity)
            self.world.setConstantValue("plcsChargeRate", [plcs.id], 0.05)
            plcs.initialize_connector_processes(default_sense_period=10, default_remote_sensor_send_period=30)

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
            vehicle.initialize_connector_processes(default_sense_period=10, default_remote_sensor_send_period=30)
        for sam in sams:
            sam.initialize_connector_processes(default_sense_period=10, default_remote_sensor_send_period=30)

    def setup_distributions(self):
        super().setup_distributions()
        transfer_starts = self.world.get_exogenous_action("transferStarts")
        # transfer_starts.config.occurrence_distribution = NormalDistribution("integer", 5, 1)
        transfer_starts.config.occurrence_distribution = MessageDelayDistribution(
            {"freeSlotsL": (1, 0)}, default=(5, 1))
        transfer_starts.config.set_param_distribution("error", ConstantDistribution("term", None))

        transfer_ends = self.world.get_exogenous_action("transferEnds")
        # transfer_ends.config.occurrence_distribution = NormalDistribution("integer", 5, 1)
        transfer_ends.config.occurrence_distribution = MessageDelayDistribution(
            {"freeSlotsL": (1, 0)}, default=(5, 1))
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

    def setup_properties(self):
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
        """.format(self.__time_limit)

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
        if self.__mode == VISUALIZE:
            self.property_collection.register_property("g", goal1, ACHIEVE)
        else:
            self.property_collection.register_property("f", fstr, INVARIANT)
            self.property_collection.register_property("g", goal2, ACHIEVE)