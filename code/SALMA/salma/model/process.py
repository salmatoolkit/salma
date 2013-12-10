import salma.model.core as core
from salma.SMCException import SMCException
from salma.model.procedure import ControlNode, ActionExecution


class Process(object):
    """
    A process has a procedure and defines its scheduling properties. Processes can either be periodic, triggered
    by some condition, or one-shot, which is defined in the corresponding subclasses.
    """

    IDLE, RUNNING, BLOCKED, EXECUTING_ACTION, TERMINATED = range(5)

    def __init__(self, agent, procedure):
        """
        Creates a process with the given procedure that is owned by the given agent.
        :type agent: core.Agent
        :type procedure: Procedure
        """
        self.__agent = agent
        self.__state = Process.IDLE
        self.__procedure = procedure
        self.__current_control_node = procedure.body
        self.__current_evaluation_context = self.agent.evaluation_context
        self.__pending_action = None

    @property
    def state(self):
        """
        The state of the process, i.e. IDLE, RUNNING
        :rtype: int
        """
        return self.__state

    def is_scheduled(self):
        """
        Returns True if the process is not idle.
        :rtype: bool
        """
        return self.__state != Process.IDLE and self.__state != Process.TERMINATED

    def is_terminated(self):
        return self.__state == Process.TERMINATED

    @property
    def agent(self):
        """
        The agent that owns this process.
        :rtype: Agent
        """
        return self.__agent

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

    def __on_start(self):
        """
        Called at start
        """
        self.state = Process.RUNNING

    def __on_finish(self):
        """
        Called at finish.
        """
        self.__state = Process.IDLE

    def start(self):
        self.__current_evaluation_context = self.agent.evaluation_context
        self.__current_control_node = self.procedure.body
        self.__pending_action = None
        self.__on_start()

    def stop(self):
        self.__current_control_node = None
        self.__current_evaluation_context = self.agent.evaluation_context
        self.__pending_action = None
        self.__on_finish()

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
            self.stop()
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
    def __init__(self, agent, procedure):
        Process.__init__(self, agent, procedure)
