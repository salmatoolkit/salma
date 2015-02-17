from numbers import Number
from .core import Entity
from salma.SALMAException import SALMAException
from salma.model.procedure import ControlNode, Act, Procedure, Wait
from salma.model.evaluationcontext import EvaluationContext

ONE_SHOT_PROCESS = 0
PERIODIC_PROCESS = 1
TRIGGERED_PROCESS = 2


class Process(object):
    """
    A process has a procedure and defines its scheduling properties. Processes can either be periodic, triggered
    by some condition, or one-shot, which is defined in the corresponding subclasses.
    """

    IDLE, RUNNING, WAITING, SLEEPING, EXECUTING_ACTION = range(5)

    def __init__(self, procedure, introduction_time=0):
        """
        Creates a process with the given procedure that is owned by the given agent.
        The status is initially set to IDLE.
        :type procedure: Procedure|ControlNode|list
        :type introduction_time: int
        """
        self.__agent = None
        self.__state = Process.IDLE

        self.__blocking_condition = None
        self.__suspended_until = None

        if isinstance(procedure, Procedure):
            self.__procedure = procedure
        elif isinstance(procedure, (ControlNode, list)):
            self.__procedure = Procedure("main", [], procedure)
        else:
            raise SALMAException("Unsupported type for process procedure.")
        self.__current_control_node = self.__procedure.body
        self.__current_evaluation_context = None
        self.__introduction_time = introduction_time
        self.__last_start_time = None
        self.__last_end_time = None
        self.__terminated = False
        self.__execution_count = 0

    @property
    def process_type(self) -> int:
        raise NotImplementedError()

    @property
    def state(self):
        """
        The state of the process, i.e. IDLE, RUNNING
        :rtype: int
        """
        return self.__state

    @property
    def introduction_time(self) -> int:
        """
        The time when the process was introduced / created.
        :rtype: int
        """
        return self.__introduction_time

    @property
    def last_start_time(self) -> int:
        return self.__last_start_time

    @property
    def last_end_time(self) -> int:
        return self.__last_end_time

    def get_deadline(self) -> int:
        raise NotImplementedError()

    @property
    def execution_count(self) -> int:
        return self.__execution_count

    def is_scheduled(self) -> bool:
        """
        Returns True if the process is not idle.
        """
        return not self.terminated and self.__state != Process.IDLE

    @property
    def terminated(self) -> bool:
        return self.__terminated

    @property
    def agent(self) -> Entity:
        """
        The agent that owns this process.
        """
        return self.__agent

    @agent.setter
    def agent(self, agent: Entity):
        self.__agent = agent
        self.__current_evaluation_context = self.__agent.evaluation_context

    @property
    def procedure(self) -> Procedure:
        """
        The procedure that defines the behaviour of the process.
        """
        return self.__procedure

    @property
    def process_id(self) -> str:
        return self.__procedure.name + "@" + self.__agent.id

    @property
    def current_evaluation_context(self) -> EvaluationContext:
        return self.__current_evaluation_context

    @property
    def blocking_condition(self):
        """
        Returns the blocking condition as tuple (condition_type, condition, params)
        :rtype: (int, object, list)
        """
        return self.__blocking_condition

    @blocking_condition.setter
    def blocking_condition(self, condition_spec):
        self.__blocking_condition = condition_spec

    @property
    def suspended_until(self):
        """
        Returns the time until which the process is suspended by a sleep node.
        :rtype: int
        """
        return self.__suspended_until

    @suspended_until.setter
    def suspended_until(self, t):
        self.__suspended_until = t

    def wake_up_if_possible(self):
        """
        Wakes the process up (i.e. sets state to RUNNING) if the blocking condition is fulfilled or
        the suspension period is over.
        :return: True if the process is running after this call
        """
        if self.state == Process.RUNNING:
            return True

        if (self.state in (Process.SLEEPING, Process.WAITING) and
                self.suspended_until is not None):
            current_time = self.current_evaluation_context.get_current_time()
            if current_time >= self.suspended_until:
                self.__state = Process.RUNNING
                self.__suspended_until = None
                return True

        if self.state == Process.WAITING and self.blocking_condition is not None:
            condition_type, condition, params = self.blocking_condition
            if self.current_evaluation_context is None:
                raise SALMAException("Undefined evaluation context in process!")
            res = self.current_evaluation_context.evaluateCondition(condition_type, condition, *params)
            if res:
                self.__state = Process.RUNNING
                self.__blocking_condition = None
                self.__suspended_until = None
                return True
        return False

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

    # noinspection PyMethodMayBeStatic
    def _on_start(self):
        """
        Called at start.
        """
        pass

    # noinspection PyMethodMayBeStatic
    def _on_finish(self):
        """
        Called at finish.
        """
        pass

    def get_next_known_start_time(self, current_time):
        """
        Returns the next known start time of the process that is later than the current time.
        :type current_time: int
        :rtype: int
        """
        raise NotImplementedError()

    def get_next_known_activation_time(self, current_time):
        """
        Returns the next known time when the process will be (re-)activated that is later than
        the current time.
        :type current_time: int
        :rtype: int
        """
        if self.state in (Process.SLEEPING, Process.WAITING):
            if self.suspended_until is not None and self.suspended_until > current_time:
                return self.suspended_until
            else:
                return None
        else:
            return self.get_next_known_start_time(current_time)

    def start(self):
        """
        Starts the process. Resets procedure and evaluation context, calls _on_start() and
        sets state=RUNNING.
        :return:
        """
        self.__current_evaluation_context = self.agent.evaluation_context
        self.procedure.restart(self.__current_evaluation_context)
        self.__current_control_node = self.procedure.body
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
        self._on_finish()
        self.__state = Process.IDLE
        if self.should_terminate():
            self.__terminated = True

    def reset(self):
        self.__state = Process.IDLE
        self.__current_control_node = self.procedure.body
        self.__current_evaluation_context = None
        self.__last_start_time = None
        self.__last_end_time = None
        self.__terminated = False
        self.__execution_count = 0

    def step(self):
        """
        Performs one step of the process.
        :return: returns an Act instance if an action is performed or None

        :rtype: Act
        """
        if self.__current_evaluation_context is None:
            raise SALMAException("No evaluation context for process " + self.process_id)

        if self.__current_control_node is None:
            return None

        if self.__state == Process.EXECUTING_ACTION:
            self.__state = Process.RUNNING
        if not self.__state == Process.RUNNING:
            return None

        status = ControlNode.CONTINUE
        current_node = self.__current_control_node
        current_context = self.__current_evaluation_context
        # TODO: set current process field in evaluation context
        while status == ControlNode.CONTINUE and current_node is not None:
            status, next_node, next_context = current_node.execute_step(current_context,
                                                                        self.__agent.procedure_registry)

            # : :type next_context: EvaluationContext
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
        action = None

        if self.__current_control_node is not None:
            # status == BLOCKED
            if isinstance(self.__current_control_node, Act):
                self.__state = Process.EXECUTING_ACTION
                action = self.__current_control_node

                # set pointer to enclosing sequence if there is one
                # otherwise set to None to finish process
                if self.__current_control_node.parent is not None:
                    self.__current_control_node = self.__current_control_node.parent
                else:
                    self.__current_control_node = None
            else:
                self.__state = Process.WAITING
                if isinstance(self.__current_control_node, Wait):
                    cond = self.__current_control_node.condition
                    cparams = self.__current_control_node.condition_params
                    ctype = self.__current_evaluation_context.determine_source_type(cond, cparams)
                    self.__blocking_condition = ctype, cond, cparams
                    self.__suspended_until = self.__current_control_node.current_timeout

        if self.__current_control_node is None:
            self.__execution_count += 1
            self.__last_end_time = self.agent.evaluation_context.getFluentValue('time')
            self.__finish()

        return action


class OneShotProcess(Process):
    """
    A process that is run only once. The get_deadline is set relative to the time the process is created.
    """

    def __init__(self, procedure, absolute_deadline=None, introduction_time=0):
        Process.__init__(self, procedure, introduction_time)
        self.__absolute_deadline = absolute_deadline

    def process_type(self) -> int:
        return ONE_SHOT_PROCESS

    def get_deadline(self):
        return self.__absolute_deadline

    def should_start(self):
        if self.execution_count > 0:
            return False
        if self.introduction_time is not None:
            current_time = self.agent.evaluation_context.getFluentValue('time')
            return current_time >= self.introduction_time
        return True

    def should_terminate(self):
        return self.execution_count > 0

    def get_next_known_start_time(self, current_time):
        min_time = self.introduction_time or 0
        if min_time > current_time:
            return min_time
        else:
            return None


class PeriodicProcess(Process):
    def __init__(self, procedure, period, introduction_time=0):
        Process.__init__(self, procedure, introduction_time)
        self.__period = period

    def process_type(self) -> int:
        return PERIODIC_PROCESS

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
        min_time = self.introduction_time or 0
        current_time = self.agent.evaluation_context.getFluentValue('time')
        assert isinstance(current_time, Number)
        if current_time < min_time:
            return None
        num = divmod(current_time - min_time, self.__period)[0] + 1
        start = min_time + (num - 1) * self.period
        end = start + self.period
        return num, start, end

    def get_deadline(self):
        """
        Returns the current absolute deadline.
        """
        min_time = self.introduction_time or 0
        return min_time + self.time_slot[0] * self.__period

    def should_start(self):
        # start if IDLE and the last start time was before the start of this time slot
        if self.period is None:
            # TODO: include process name
            raise SALMAException("Unspecified period for periodic process.")

        if (self.state != Process.IDLE) or (self.time_slot is None):
            return False

        return (self.last_start_time is None
                or self.last_start_time < self.time_slot[1])

    def should_terminate(self):
        return False

    def get_next_known_start_time(self, current_time):
        min_time = self.introduction_time or 0
        if min_time > current_time:
            return min_time
        return min_time + self.time_slot[0] * self.__period


class TriggeredProcess(Process):
    def __init__(self, procedure, condition, condition_params=None, relative_deadline=None,
                 introduction_time=0):
        Process.__init__(self, procedure, introduction_time)
        self.__condition = condition
        self.__condition_params = condition_params if condition_params is not None else []
        self.__relative_deadline = relative_deadline

    def process_type(self) -> int:
        return TRIGGERED_PROCESS

    def should_start(self):
        min_time = self.introduction_time or 0
        current_time = self.agent.evaluation_context.getFluentValue('time')
        if current_time < min_time:
            return False
        ectx = self.current_evaluation_context or self.agent.evaluation_context
        condition_type = ectx.determine_source_type(self.__condition, self.__condition_params)
        result = ectx.evaluateCondition(condition_type,
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
            min_time = self.introduction_time or 0
            return min_time + self.__relative_deadline
        else:
            return self.last_start_time + self.__relative_deadline

    def should_terminate(self):
        return False

    def get_next_known_start_time(self, current_time):
        min_time = self.introduction_time or 0
        if min_time > current_time:
            return min_time
        else:
            return None












