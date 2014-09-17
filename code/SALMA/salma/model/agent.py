from .core import Entity
from .procedure import ProcedureRegistry, Procedure, Sense, Send, Receive, SetFluent
from salma.SALMAException import SALMAException
from .evaluationcontext import EvaluationContext
from salma.model import process
from salma.model.process import OneShotProcess, PeriodicProcess
from salma.model.infotransfer import Connector, Channel, Sensor, RemoteSensor
from collections.abc import Iterable

class Agent(Entity):
    """
    An agent is an active entity that executes one or several processes.
    """

    def __init__(self, entity_id, sort_name, processes=[], procedure_registry=None, automatic_info_transfer=True):
        """
        Creates an agent with the given id, sort and control procedure. Additionally,
        a procedure registry can be specified to allow procedure calls within the agent's control
        procedure.

        :type entity_id: str
        :type sort_name: str
        :type processes: object
        :type procedure_registry: ProcedureRegistry
        :type automatic_info_transfer: bool
        """
        Entity.__init__(self, entity_id, sort_name)
        self.__evaluation_context = None
        #: :type : list[process.Process]
        self.__processes = []
        #: :type : dict[str, Process]
        self.__sensor_processes = dict()

        #: :type : dict[str, Process]
        self.__remote_sensor_src_processes = dict()

        #: :type : dict[str, Process]
        self.__remote_sensor_dest_processes = dict()

        self.__automatic_info_transfer = automatic_info_transfer
        if isinstance(processes, Procedure):
            self.add_process(OneShotProcess(processes))
        elif isinstance(processes, list):
            for p in processes:
                self.add_process(p)

        self.evaluation_context = None
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
        :rtype: list of process.Process
        """
        return self.__processes

    @property
    def automatic_info_transfer(self) -> bool:
        return self.__automatic_info_transfer

    def add_process(self, p):
        """
        Adds a process to the agent's registry.

        :type p: process.Process
        """
        self.__processes.append(p)
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
        :rtype: list of process.Process
        """
        if self.evaluation_context is None:
            raise SALMAException("No evaluation context for agent " + self.id)

        if self.procedure_registry is None:
            raise SALMAException("No procedure registry given for agent " + self.id)

        #: :type: list of process.Process
        running_processes = []

        for p in self.processes:
            if not p.terminated:
                if p.state == process.Process.IDLE and p.should_start():
                    p.start()
                if p.is_scheduled():
                    running_processes.append(p)

        return running_processes

    def init_info_transfer(self, channels, sensors, remote_sensors):
        """

        :param Iterable[Channel] channels: the declared channels
        :param Iterable[Sensor] sensors: the declared sensors
        :param Iterable[RemoteSensor] remote_sensors: the declared remote sensors
        """
        if not self.automatic_info_transfer:
            return

        for s in sensors:
            if s.owner_type == self.sortName:
                proc = Procedure("main", [],
                                 [
                                     Sense(s.name, [])
                                 ])
                p = PeriodicProcess(proc, 5)
                self.add_process(p)
                self.__sensor_processes[s.name] = p

        # todo:
        for rs in remote_sensors:
            if rs.local_sensor_owner_type == self.sortName:
                proc = Procedure("main", [],
                                 [

                                 ])



