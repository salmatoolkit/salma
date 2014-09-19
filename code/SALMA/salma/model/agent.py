from .core import Entity
from .procedure import ProcedureRegistry, Procedure, Sense
from salma.SALMAException import SALMAException
from .evaluationcontext import EvaluationContext
from salma.model import process
from salma.model.infotransfer import Sensor, RemoteSensor
from salma.model.procedure import TransmitRemoteSensorReading, UpdateRemoteSensor
from salma.model.process import OneShotProcess, PeriodicProcess, Process, TriggeredProcess
from salma.model.world_declaration import WorldDeclaration


class Agent(Entity):
    """
    An agent is an active entity that executes one or several processes.
    """

    def __init__(self, entity_id, sort_name, processes=[], procedure_registry=None,
                 world_declaration=None):
        """
        Creates an agent with the given id, sort and control procedure. Additionally,
        a procedure registry can be specified to allow procedure calls within the agent's control
        procedure.

        :type entity_id: str
        :type sort_name: str
        :type processes: object
        :type procedure_registry: ProcedureRegistry
        :type world_declaration: WorldDeclaration
        :type automatic_info_transfer: bool
        """
        Entity.__init__(self, entity_id, sort_name)
        self.__evaluation_context = None
        # : :type : set[process.Process]
        self.__processes = set()
        # : :type : dict[str, Process]
        self.__sensor_processes = dict()

        # : :type : dict[str, Process]
        self.__remote_sensor_src_processes = dict()

        # : :type : dict[str, Process]
        self.__remote_sensor_dest_processes = dict()

        if isinstance(processes, Procedure):
            self.add_process(OneShotProcess(processes))
        elif isinstance(processes, list):
            for p in processes:
                self.add_process(p)

        self.evaluation_context = None
        self.__world_declaration = world_declaration
        self.__procedure_registry = procedure_registry or ProcedureRegistry()

    @property
    def evaluation_context(self):
        """
        The evaluation context used by this entity.
        :rtype: EvaluationContext
        """
        return self.__evaluation_context

    @evaluation_context.setter
    def evaluation_context(self, ctx):
        """
        :type ctx: EvaluationContext
        """
        self.__evaluation_context = ctx
        if ctx is not None:
            ctx.setAgent(self)
            ctx.setProcedureCall(None)

    @property
    def procedure_registry(self):
        """
        The agent's procedure registry.
        :rtype: ProcedureRegistry
        """
        return self.__procedure_registry

    @property
    def processes(self):
        """
        The agent's processes.
        :rtype: set[process.Process]
        """
        return self.__processes

    @property
    def local_sensor_processes(self):
        """
        The processes that perform sensing for all declared local sensors of this agent type.
        :rtype: dict[str, process.Process]
        """
        return self.__sensor_processes

    def set_local_sensor_process(self, sensor_name: str, sensor_process: Process):
        if self.__world_declaration is None:
            raise SALMAException("No world declaration specified for agent {}.".format(self.id))
        con = self.__world_declaration.get_connector(sensor_name)
        if con is None:
            raise SALMAException("No connector specified with name {}.".format(sensor_name))
        if not isinstance(con, Sensor):
            raise SALMAException("Connector {} is not a sensor.".format(sensor_name))

        if sensor_name in self.__sensor_processes:
            self.__processes.remove(self.__sensor_processes[sensor_name])
        if sensor_process in self.__processes:
            raise SALMAException(
                "Trying to add already used process for local sensor {} of agent {}".format(sensor_name, self.id))

        self.__processes.add(sensor_process)
        self.__sensor_processes[sensor_name] = sensor_process

    def set_local_sensor_period(self, sensor_name, period):
        proc = Procedure("main", [],
                         [
                             Sense(sensor_name, [])
                         ])

        p = PeriodicProcess(proc, period)
        self.set_local_sensor_process(sensor_name, p)

    @property
    def remote_sensor_src_processes(self):
        """
        The processes that
        :rtype: dict[str, process.Process]
        """
        return self.__remote_sensor_src_processes

    def set_remote_sensor_src_process(self, sensor_name: str, sensor_process: Process):
        if self.__world_declaration is None:
            raise SALMAException("No world declaration specified for agent {}.".format(self.id))
        con = self.__world_declaration.get_connector(sensor_name)
        if con is None:
            raise SALMAException("No connector specified with name {}.".format(sensor_name))
        if not isinstance(con, RemoteSensor):
            raise SALMAException("Connector {} is not a remote sensor.".format(sensor_name))

        if sensor_name in self.__remote_sensor_src_processes:
            self.__processes.remove(self.__remote_sensor_src_processes[sensor_name])
        if sensor_process in self.__processes:
            raise SALMAException(
                "Trying to add already used process for remote sensor source {} of agent {}".format(sensor_name,
                                                                                                    self.id))
        self.__processes.add(sensor_process)
        self.__remote_sensor_src_processes[sensor_name] = sensor_process

    def set_remote_sensor_send_period(self, sensor_name: str, period: int):
        proc = Procedure("main", [],
                         [
                             # TODO: think about how to deal with parameters
                             TransmitRemoteSensorReading(sensor_name)
                         ])
        p = PeriodicProcess(proc, period)
        self.set_remote_sensor_src_process(sensor_name, p)

    @property
    def remote_sensor_dest_processes(self):
        """
        :rtype: dict[str, process.Process]
        """
        return self.__remote_sensor_dest_processes

    def set_remote_sensor_dest_process(self, sensor_name: str, sensor_process: Process):
        if self.__world_declaration is None:
            raise SALMAException("No world declaration specified for agent {}.".format(self.id))
        con = self.__world_declaration.get_connector(sensor_name)
        if con is None:
            raise SALMAException("No connector specified with name {}.".format(sensor_name))
        if not isinstance(con, RemoteSensor):
            raise SALMAException("Connector {} is not a remote sensor.".format(sensor_name))

        if sensor_name in self.__remote_sensor_dest_processes:
            self.__processes.remove(self.__remote_sensor_dest_processes[sensor_name])
        if sensor_process in self.__processes:
            raise SALMAException(
                "Trying to add already used process for remote sensor source {} of agent {}".format(sensor_name,
                                                                                                    self.id))
        self.__processes.add(sensor_process)
        self.__remote_sensor_dest_processes[sensor_name] = sensor_process

    def set_remote_sensor_update_period(self, sensor_name: str, period: int):
        proc = Procedure("main", [],
                         [
                             UpdateRemoteSensor(sensor_name)
                         ])
        p = PeriodicProcess(proc, period)
        self.set_remote_sensor_dest_process(sensor_name, p)

    def make_remote_sensor_update_triggered(self, sensor_name: str):
        proc = Procedure("main", [],
                         [
                             UpdateRemoteSensor(sensor_name)
                         ])
        p = TriggeredProcess(proc, EvaluationContext.TRANSIENT_FLUENT,
                             "message_available", [Entity.SELF, sensor_name, sensor_name])
        self.set_remote_sensor_dest_process(sensor_name, p)

    def add_process(self, p: Process):
        """
        Adds a process to the agent's registry.
        """
        self.__processes.add(p)
        p.agent = self

    def restart(self):
        """
        Stops all of the agent's processes.
        """
        for p in self.processes:
            p.reset()

    def is_finished(self):
        for p in self.processes:
            if not p.terminated:
                return False
        return True

    def update_schedule(self):
        """
        Updates the current schedule and returns the list of currently executed processes.
        :rtype: list[process.Process]
        """
        if self.evaluation_context is None:
            raise SALMAException("No evaluation context for agent " + self.id)

        if self.procedure_registry is None:
            raise SALMAException("No procedure registry given for agent " + self.id)

        # : :type: list of process.Process
        running_processes = []

        for p in self.processes:
            if not p.terminated:
                if p.state == process.Process.IDLE and p.should_start():
                    p.start()
                if p.is_scheduled():
                    running_processes.append(p)

        return running_processes

    def initialize_connector_processes(self, mode_remote_sensor_src=process.PERIODIC_PROCESS,
                                       mode_remote_sensor_dest=process.TRIGGERED_PROCESS, default_sense_period=5,
                                       default_remote_sensor_send_period=5, default_remote_sensor_update_period=5):
        """
        Initializes background processes for information transfer according to connector declarations. Using the
        parameters, both periodic and triggered processes can be installed. For periodic processes, default periods
        can be given.

        :param int mode_remote_sensor_src: mode to use for either process.PERIODIC_PROCESS or process.TriggeredProcess

        """
        if self.__world_declaration is None:
            raise SALMAException("No world declaration specified for agent {}.".format(self.id))

        sensors = self.__world_declaration.get_sensors()
        for s in sensors:
            if s.owner_type == self.sortName:
                self.set_local_sensor_period(s.name, default_sense_period)
        remote_sensors = self.__world_declaration.get_remote_sensors()
        for rs in remote_sensors:
            if rs.local_sensor_owner_type == self.sortName:
                self.set_remote_sensor_send_period(rs.name, default_remote_sensor_send_period)
                # TODO: allow triggered processes that react to changes / updates

            if rs.remote_sensor_owner_type == self.sortName:
                if mode_remote_sensor_dest == process.PERIODIC_PROCESS:
                    self.set_remote_sensor_update_period(rs.name, default_remote_sensor_update_period)
                else:
                    self.make_remote_sensor_update_triggered(rs.name)