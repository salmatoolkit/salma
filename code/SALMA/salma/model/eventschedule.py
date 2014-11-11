import random
from salma.engine import Engine
from salma.model.actions import *
import logging

MODULE_LOGGER_NAME = 'salma.model'
moduleLogger = logging.getLogger(MODULE_LOGGER_NAME)


class EventSchedule:

    def __init__(self, logics_engine):
        """
        Creates an instance of the event schedule.

        :param Engine logics_engine: the logics engine to use
        """
        self.__logics_engine = logics_engine
        #: :type: list[(int, ExogenousAction, list)]
        self.__event_schedule = []
        #: :type: list[(int, ExogenousAction, list)]
        self.__possible_event_schedule = []
        #: :type: list[(int, ExogenousAction, list)]
        self.__schedulable_event_schedule = []

    # def get_exogenous_action_instances(self):
    #     """
    #     Creates a list of all exogenous action instances (see ExogenusAction.generate_instance) for
    #     all possible candidates. The list of possible candidates is calculated by calling
    #     Engine.getExogenousActionCandidates() first. Then ExogenousAction.shouldHappen() is used to
    #     select instances that should occur.
    #     :rtype: list
    #     """
    #     #candidates = World.logic_engine().get_currently_possible_ad_hoc_event_instances()
    #     ea_instances = []
    #     for candidate in candidates.items():
    #         action_name = candidate[0]
    #         combinations = candidate[1]
    #         # just ignore if the ex action is not registered
    #         if action_name in self.__exogenousActions:
    #             ea = self.__exogenousActions[action_name]
    #             for params in combinations:
    #                 if ea.should_happen(self.__evaluationContext, params):
    #                     instance = ea.generate_instance(self.__evaluationContext, params)
    #                     ea_instances.append(instance)
    #     return ea_instances

    def update_event_schedule(self, limit=None):
        current_time = self.getTime()
        if limit is None:
            limit = current_time
        poss_events = self.__logics_engine.get_next_possible_ad_hoc_event_instances(limit)
        if len(poss_events) > 0:
            if (len(self.__possible_event_schedule) == 0 or
                    self.__possible_event_schedule[0][0] > poss_events[0][0]):
                self.__possible_event_schedule = poss_events
            elif self.__possible_event_schedule[0][0] == poss_events[0][0]:
                self.__possible_event_schedule.extend(poss_events)
            # else: newly found possible events are later than the currently known --> do nothing

        # check if we can add any possible event

        pass

    def progress_interleaved(self, action_instances):
        """
        Progresses the given action / event instances in random order. For each instance of stochastic actions,
        an outcome is generated at the time it is due to be progressed.

        : param action_instances: :
            action instances = tuples (action, arguments, EvaluationContext) where the last EvaluationContext argument
            is only used for stochastic actions
        :type action_instances: list[(Action|ExogenousAction, list, EvaluationContext)]
        :return: list of failed actions / events
        :rtype: list[str, list]
        """
        #TODO: consider caused / triggered events
        if len(action_instances) == 0:
            return []
        failed = []
        random.shuffle(action_instances)
        # progress deterministic action as batch but generate outcome for stochastic actions ad hoc
        act_seq = []
        performed_actions = []
        for ai in action_instances:
            act = ai[0]

            if isinstance(act, DeterministicAction):
                action_name = act.name
                args = ai[1]
                act_seq.append((action_name, args))
            elif isinstance(act, ExogenousAction):
                # note: for exogenous actions, the evaluation context is ignored
                action_name = act.action_name
                args = ai[1]
                act_seq.append((action_name, args))
            elif isinstance(act, StochasticAction):
                if len(act_seq) > 0:
                    fa = self.__logics_engine.progress(act_seq)
                    performed_actions.extend(act_seq)
                    failed.extend(fa)
                    act_seq.clear()
                action_name, args = act.generateOutcome(ai[2], ai[1])
                fa = self.__logics_engine.progress([(action_name, args)])
                failed.extend(fa)
                performed_actions.append((action_name, args))
            else:
                raise SALMAException("Unsupported action instance: {}".format(type(act)))
        if len(act_seq) > 0:
            fa = self.__logics_engine.progress(act_seq)
            failed.extend(fa)
            performed_actions.extend(act_seq)
        if moduleLogger.isEnabledFor(logging.DEBUG):
            moduleLogger.debug("  Progressed: %s, failed: ", performed_actions, failed)
        return performed_actions, failed

    def __schedule_possible_actions(self):
        pass

