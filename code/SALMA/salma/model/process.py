from .core import Entity
from salma.SMCException import SMCException
from salma.model.procedure import ControlNode, ActionExecution, Procedure
from salma.model.evaluationcontext import EvaluationContext


class Process(object):
    """
    A process has a procedure and defines its scheduling properties. Processes can either be periodic, triggered
    by some condition, or one-shot, which is defined in the corresponding subclasses.
    """

    IDLE, RUNNING, BLOCKED, EXECUTING_ACTION = range(4)

    def __init__(self, procedure, introduction_time=0):
        """
        Creates a process with the given procedure that is owned by the given agent.
        The status is initially set to IDLE.
        :type procedure: Procedure
        :type introduction_time: int
        """
        self.__agent = None
        self.__state = Process.IDLE
        self.__procedure = procedure
        self.__current_control_node = procedure.body
        self.__current_evaluation_context = None
        self.__pending_action = None
        self.__introduction_time = introduction_time
        self.__last_start_time = None
        self.__last_end_time = None
        self.__terminated = False
        self.__execution_count = 0

    @property
    def state(self):
        """
        The state of the process, i.e. IDLE, RUNNING
        :rtype: int
        """
        return self.__state

    @property
    def introduction_time(self):
        """
        The time when the process was introduced / created.
        :rtype: int
        """
        return self.__introduction_time

    @property
    def last_start_time(self):
        return self.__last_start_time

    @property
    def last_end_time(self):
        return self.__last_end_time

    def get_deadline(self):
        raise NotImplementedError()

    @property
    def execution_count(self):
        return self.__execution_count

    def is_scheduled(self):
        """
        Returns True if the process is not idle.
        :rtype: bool
        """
        return not self.terminated and self.__state != Process.IDLE

    @property
    def terminated(self):
        return self.__terminated

    @property
    def agent(self):
        """
        The agent that owns this process.
        :rtype: Entity
        """
        return self.__agent

    @agent.setter
    def agent(self,  agent):
        """
        :type agent: Entity
        """
        self.__agent = agent
        self.__current_evaluation_context = self.__agent.evaluation_context

    @property
    def procedure(self):
        """
        The procedure that defines the behaviour of the process.
         :rtype: Procedure
        """
        return self.__procedure

    @property
    def process_id(self):
        return self.__procedure.name + "@" + self.__agent.id

    @property
    def current_evaluation_context(self):
        """
        :rtype: EvaluationContext
        """
        return self.__current_evaluation_context

    def get_pending_action(self):
        """
        Returns, if any, the action that was yielded by the agent during the immediate action gathering phase.
         This action is added to the non-immediate action list in World.step and will thus be executed after the
         immediate action phase.
        :rtype: (str, list)
        """
        return self.__pending_action

    def set_pending_action(self, pending_action):
        """
        :type pending_action: (str, list)
        """
        self.__pending_action = pending_action

    # TEMPLATE METHODS

    def should_start(self):
        """
        Returns true if the process should be started.
        :rtype: bool
        """
        raise NotImplementedError()

    def should_terminate(self):
        """
        Returns true if the process should be terminated. Called in stop().
        :rtype: bool
        """
        return False

    def _on_start(self):
        """
        Called at start.
        """
        pass

    def _on_finish(self):
        """
        Called at finish.
        """
        pass

    def start(self):
        """
        Starts the process. Resets procedure and evaluation context, calls _on_start() and
        sets state=RUNNING.
        :return:
        """
        self.__current_evaluation_context = self.agent.evaluation_context
        self.procedure.restart(self.__current_evaluation_context)
        self.__current_control_node = self.procedure.body
        self.__pending_action = None
        self.__last_start_time = self.agent.evaluation_context.getFluentValue('time')

        self._on_start()
        self.__state = Process.RUNNING

    def __finish(self):
        """
        Stops the process. Resets procedure and evaluation context, calls _on_finish() and
        sets state=IDLE.
        :return:
        """
        self.__current_control_node = None
        self.__current_evaluation_context = self.agent.evaluation_context
        self.__pending_action = None
        self._on_finish()
        self.__state = Process.IDLE
        if self.should_terminate():
            self.__terminated = True

    def reset(self):
        self.__state = Process.IDLE
        self.__current_control_node = self.procedure.body
        self.__current_evaluation_context = None
        self.__pending_action = None
        self.__last_start_time = None
        self.__last_end_time = None
        self.__terminated = False
        self.__execution_count = 0

    def step(self, new_step):
        """
        Performs one step of the process.

        :rtype: ActionExecution
        """
        if self.__current_evaluation_context is None:
            raise SMCException("No evaluation context for process " + self.process_id)

        if self.__current_control_node is None:
            return None

        if self.__pending_action is not None and new_step is False:
            return None
        self.__state = Process.RUNNING
        self.__pending_action = None

        status = ControlNode.CONTINUE
        current_node = self.__current_control_node
        current_context = self.__current_evaluation_context
        while status == ControlNode.CONTINUE and current_node is not None:
            status, next_node, next_context = current_node.executeStep(current_context, self.__agent.procedure_registry)

            #: :type next_context: EvaluationContext
            if next_node is None:
                if current_node.parent is None:
                    # this means we just left some procedure
                    if next_context.getProcedureCall() is not None:
                        next_node = next_context.getProcedureCall().parent
                        next_context = next_context.getParent()

                else:
                    next_node = current_node.parent

            current_node = next_node
            current_context = next_context

        self.__current_control_node = current_node
        self.__current_evaluation_context = current_context

        if self.__current_control_node is None:
            self.__execution_count += 1
            self.__last_end_time = self.agent.evaluation_context.getFluentValue('time')
            self.__finish()
            return None
        else:
        # status == BLOCKED
            if isinstance(self.__current_control_node, ActionExecution):
                self.__state = Process.EXECUTING_ACTION
                action = self.__current_control_node

                # set pointer to enclosing sequence if there is one
                if self.__current_control_node.parent is not None:
                    self.__current_control_node = self.__current_control_node.parent
                return action  # return action
            else:
                self.__state = Process.BLOCKED
                return None


class OneShotProcess(Process):
    """
    A process that is run only once. The get_deadline is set relative to the time the process is created.
    """
    def __init__(self, procedure, absolute_deadline=None, introduction_time=0):
        Process.__init__(self, procedure, introduction_time)
        self.__absolute_deadline = absolute_deadline

    def get_deadline(self):
        return self.__absolute_deadline

    def should_start(self):
        return self.execution_count == 0

    def should_terminate(self):
        return self.execution_count > 0


class PeriodicProcess(Process):

    def __init__(self, procedure, period, introduction_time=0):
        Process.__init__(self, procedure, introduction_time)
        self.__period = period

    @property
    def period(self):
        return self.__period

    @period.setter
    def period(self, p):
        self.__period = p

    @property
    def time_slot(self):
        """
        Returns the current timeslot as a tuple of (timeslot_num, start, end)
        :rtype (int, int, int)
        """
        current_time = self.agent.evaluation_context.getFluentValue('time')
        num = divmod(current_time, self.__period)[0] + 1
        start = (num - 1)*self.period
        end = start + self.period
        return num, start, end

    def get_deadline(self):
        """
        Returns the current absolute get_deadline.
        """
        return self.time_slot * self.__period

    def should_start(self):
        # start if IDLE and the last start time was before the start of this time slot

        return (self.state == Process.IDLE) and (
                self.last_start_time is None
                or
                self.last_start_time < self.time_slot[1])

    def should_terminate(self):
        return False


class TriggeredProcess(Process):

    def __init__(self, procedure, condition_type, condition, condition_params, relative_deadline=None, introduction_time=0):
        Process.__init__(self, procedure, introduction_time)
        self.__condition_type = condition_type
        self.__condition = condition
        self.__condition_params = condition_params
        self.__relative_deadline = None

    def should_start(self):
        ectx = self.current_evaluation_context or self.agent.evaluation_context
        result = ectx.evaluateCondition(self.__condition_type,
                                        self.__condition,
                                        *self.__condition_params)
        return result

    @property
    def relative_deadline(self):
        return self.__relative_deadline

    def get_deadline(self):
        if self.__relative_deadline is None:
            return None
        elif self.last_start_time is None:
            return self.__relative_deadline
        else:
            return self.last_start_time + self.__relative_deadline

    def should_terminate(self):
        return False













