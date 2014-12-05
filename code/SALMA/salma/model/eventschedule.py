from salma.engine import Engine
from salma.model.actions import *
import logging
import heapq
from salma.model.actions import ExogenousAction
from salma.model.evaluationcontext import EvaluationContext
from salma.mathutils import min_robust

MODULE_LOGGER_NAME = 'salma.model'
moduleLogger = logging.getLogger(MODULE_LOGGER_NAME)


class EventSchedule:
    def __init__(self, logics_engine):
        """
        Creates an instance of the event schedule.

        :param Engine logics_engine: the logics engine to use
        """
        self.__logics_engine = logics_engine

        # name -> exogenous action
        #: :type : dict[str, ExogenousAction]
        self.__exogenous_actions = dict()

        # the event schedule contains entries of the form (time, (event, qualifying params)).
        # The final instance including stochastic params is generated in the main loop in the
        # step just before execution of the event. This means that the distribution fo the stochastic parameters
        # refer to tha scheduled time.
        #
        #: :type: list[(int, (ExogenousAction, tuple))]
        self.__event_schedule = []
        #: :type: list[(int, (ExogenousAction, tuple))]
        self.__possible_event_schedule = []
        #: :type: list[(int, (ExogenousAction, tuple))]
        self.__schedulable_event_schedule = []
        #: :type: set[(int, (ExogenousAction, tuple))]
        self.__already_processed_events = set()
        #: :type: int
        self.__last_processed_timestep = -1

    def clear(self):
        self.__exogenous_actions.clear()
        self.reset()

    def reset(self):
        self.__event_schedule.clear()
        self.__possible_event_schedule.clear()
        self.__schedulable_event_schedule.clear()
        self.__already_processed_events.clear()
        self.__last_processed_timestep = -1

    @property
    def exogenous_actions(self):
        """
        A dictionary containing all registered exogenous actions indexed by their name.
        :rtype: dict[str, ExogenousAction]
        """
        return self.__exogenous_actions

    def add_exogenous_action(self, exogenous_action):
        """
        Registers the given exogenous action. Normally this method is called automatically by World.load_declarations().
        :type exogenous_action: ExogenousAction
        """
        self.__exogenous_actions[exogenous_action.action_name] = exogenous_action

    def __process_schedulable_and_possible(self, current_time, evaluation_context):
        """
        :param int current_time: the current time step
        :param EvaluationContext evaluation_context: the evaluation context used for the occurrence distributions.
        """
        # check if we can add any possible event
        for pe in self.__possible_event_schedule:
            if (pe[0] == current_time
                    and pe not in self.__already_processed_events
                    and pe not in self.__event_schedule):
                event = pe[1][0]
                params = pe[1][1]
                new_ev = (current_time, (event, params))
                if event.should_happen(evaluation_context, params):
                    heapq.heappush(self.__event_schedule, new_ev)
                self.__already_processed_events.add(new_ev)

        for se in self.__schedulable_event_schedule:
            if (se[0] == current_time
                    and se not in self.__already_processed_events
                    and se not in self.__event_schedule):
                event = se[1][0]
                params = se[1][1]
                schedule_time = event.get_next_occurrence_time(evaluation_context, params)
                if schedule_time is not None:
                    assert schedule_time >= current_time
                    new_ev = (schedule_time, (event, params))
                    heapq.heappush(self.__event_schedule, new_ev)
                # mark the fact that we sampled the occurrence distribution regardless of
                # whether or not it's been scheduled
                self.__already_processed_events.add((current_time, (event, params)))

    def update_event_schedule(self, current_time, evaluation_context, scan, scan_start=None, scan_time_limit=None):
        """
        Scans for all possible and schedulable events up to the given limit.
        :param int current_time: the world's current time
        :param EvaluationContext evaluation_context: the evaluation context that will be used to evaluate distributions
        :param bool scan: whether to scan for new schedulable / ad hoc events
        :param int scan_start: when to start scanning
        :param int scan_time_limit: the time limit at which the search for possible and schedulable events will stop
        """
        if scan_start is None:
            scan_start = current_time
        if scan_time_limit is None:
            scan_time_limit = current_time
        if current_time > self.__last_processed_timestep:
            self.__already_processed_events.clear()
            self.__last_processed_timestep = current_time

        # schedule all event instances that are known to be possible / schedulable at this step
        self.__process_schedulable_and_possible(current_time, evaluation_context)
        if scan:
            self.__possible_event_schedule.clear()
            self.__schedulable_event_schedule.clear()
            processed = self.__translate_event_instances_to_raw(self.__already_processed_events)
            sched = self.__translate_event_instances_to_raw(self.__event_schedule)

            # check whether new event instances are possible
            poss_events = self.__logics_engine.get_next_possible_ad_hoc_event_instances(scan_start, scan_time_limit,
                                                                                        processed)
            self.__translate_event_instances_from_raw(poss_events, self.__possible_event_schedule)

            # check whether new event instances can be scheduled
            schedulable_events = self.__logics_engine.get_next_schedulable_event_instances(
                scan_start, scan_time_limit, sched, processed)
            self.__translate_event_instances_from_raw(schedulable_events, self.__schedulable_event_schedule)

    def progress_interleaved(self, evaluation_context, action_instances):
        """
        Progresses the given action / event instances in random order. For each instance of stochastic actions,
        an outcome is generated at the time it is due to be progressed.

        :param EvaluationContext evaluation_context: the evaluation context that will be used
            for sampling in distributions.
        :param action_instances:
            action instances = tuples (action, arguments, EvaluationContext) where the last EvaluationContext argument
            is only used for stochastic actions
        :type action_instances: list[(Action|ExogenousAction, list, EvaluationContext)]
        :return: tuple containing a a list of performed and a list of failed actions / events
        :rtype: (list[str, list], list[str, list])
        """
        # TODO: consider caused / triggered events
        if len(action_instances) == 0:
            return [], []
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
                # Note: for exogenous actions, the evaluation context is used that is
                # given as a parameter. This is usually the world's global context.
                #
                action_name, args = act.generate_instance(evaluation_context, ai[1])
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
            moduleLogger.debug("  Progressed: %s, failed: %s", performed_actions, failed)
        return performed_actions, failed

    def __translate_event_instances_from_raw(self, raw_event_instances, schedule):
        """
        Translates event tuples retrieved from the logics engine to tuples that refer to ExogenousAction entries.

        :param list[(int, str, tuple)] raw_event_instances: the event instances gathered by the engine
        :param list[(int, (ExogenousAction, tuple))] schedule: the event schedule heap in which the translated event
                instances will be pushed.
        """
        for rev in raw_event_instances:
            try:
                ea = self.__exogenous_actions[rev[1]]
                heapq.heappush(schedule, (rev[0], (ea, rev[2])))
            except KeyError:
                raise SALMAException("Unregistered exogenous action: {}".format(rev[1]))

    def __translate_event_instances_to_raw(self, schedule):
        """
        Translates a generator for conversion of the current event schedule to a list of event tuples of form
        (time, action_name param_values).

        :param set[(int, (ExogenousAction, tuple))]|list[(int, (ExogenousAction, tuple))] schedule: the event schedule.
        :rtype: list[(int, str, tuple)]
        """
        return ((ev[0], ev[1][0].action_name, ev[1][1]) for ev in schedule)

    def check_exogenous_action_initialization(self):
        """
        Checks whether all registered exogenous actions are properly configured.
        :return: a list of tuples of form (exo_action, problems)
        :rtype: list[(ExogenousActions, list)]
        """
        problematic_exogenous_actions = []
        for exo_action in self.__exogenous_actions.values():
            assert isinstance(exo_action, ExogenousAction)
            if exo_action.config is None:
                problematic_exogenous_actions.append((exo_action, ["uninitialized"]))
            else:
                problems = exo_action.config.check()
                if len(problems) > 0:
                    problematic_exogenous_actions.append((exo_action, problems))
        return problematic_exogenous_actions

    def get_due_events(self, current_time):
        """
        Returns all events that are due at the current time (given as a parameter).

        :param int current_time: the current time step
        :return: a list of tuples of form (event, params)
        :rtype: list[(ExogenousAction, tuple)]
        """
        due_events = []
        while len(self.__event_schedule) > 0 and self.__event_schedule[0][0] <= current_time:
            entry = heapq.heappop(self.__event_schedule)
            due_events.append(entry[1])
        return due_events

    def get_next_time_checkpoint(self):
        """
        Returns the next time step that must be visited in the simulation because at that time
        a) an event is already scheduled, b) an ad-hoc becomes possible, or
          c) an event becomes schedulable.

        :rtype: int
        """
        t1 = self.__event_schedule[0][0] if len(self.__event_schedule) > 0 else None
        t2 = self.__possible_event_schedule[0][0] if len(self.__possible_event_schedule) > 0 else None
        t3 = self.__schedulable_event_schedule[0][0] if len(self.__schedulable_event_schedule) > 0 else None
        return min_robust([t1, t2, t3])

    def get_next_scheduled_event(self):
        """
        Returns the next scheduled event instance as a tuple of the form (time, (event, params)) or
        None if no event is currently scheduled.
        :rtype: (int, (ExogenousAction, list))
        """
        if len(self.__event_schedule) > 0:
            return self.__event_schedule[0]
        else:
            return None