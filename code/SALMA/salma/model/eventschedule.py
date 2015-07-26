import logging
import heapq

from salma.engine import Engine
from salma.model.core import translate_entities
from salma.model.actions import *
from salma.model.distributions import NEVER, DONT_OCCUR
from salma.model.events import ExogenousActionChoice
from salma.model.events import ExogenousAction, EventOccurrence
from salma.model.evaluationcontext import EvaluationContext
from salma.mathutils import min_robust
import random
from collections.abc import Iterable

logger = logging.getLogger(__name__)


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
        #: :type : dict[str, ExogenousActionChoice]
        self.__exogenous_action_chooices = dict()

        # the event schedule contains entries of the form (time, (event, qualifying params)).
        # The final instance including stochastic params is generated in the main loop in the
        # step just before execution of the event. This means that the distribution fo the stochastic parameters
        # refer to tha scheduled time.
        #
        #: :type: list[EventOccurrence]
        self.__event_schedule = []
        #: :type: list[EventOccurrence]
        self.__possible_event_schedule = []
        #: :type: list[EventOccurrence]
        self.__schedulable_event_schedule = []
        #: :type: set[EventOccurrence]
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

    @property
    def exogenous_action_choices(self):
        """
        A dictionary containing all registered exogenous action choices indexed by their name.
        :rtype: dict[str, ExogenousActionChoice]
        """
        return self.__exogenous_action_chooices

    def add_exogenous_action(self, exogenous_action):
        """
        Registers the given exogenous action. Normally this method is called automatically by
        World.load_declarations().
        :type exogenous_action: ExogenousAction
        """
        self.__exogenous_actions[exogenous_action.action_name] = exogenous_action

    def add_exogenous_action_choice(self, choice):
        """
        Registers the given exogenous action choice. Normally this method is called automatically by
        World.load_declarations().
        :type choice: ExogenousActionChoice
        """
        self.__exogenous_action_chooices[choice.choice_name] = choice

    def __option_already_scheduled(self, time, exogenous_action_choice, params):
        """
        :param int time: time
        :param ExogenousActionChoice exogenous_action_choice: choice
        :param tuple params: params
        """
        for option in exogenous_action_choice.options:
            option_instance = EventOccurrence(time, option, params)
            if option_instance in self.__event_schedule:
                return True
        return False

    def __process_schedulable_and_possible(self, current_time, evaluation_context):
        """
        :param int current_time: the current time step
        :param EvaluationContext evaluation_context: the evaluation context used for the occurrence distributions.
        """
        # check if we can add any possible event
        for pe in self.__possible_event_schedule:

            if (pe.time_point == current_time
                and pe not in self.__already_processed_events):
                if isinstance(pe.event, ExogenousAction):
                    if pe not in self.__event_schedule:
                        if pe.event.should_happen(evaluation_context, pe.params):
                            heapq.heappush(self.__event_schedule, pe)
                        self.__already_processed_events.add(pe)
                else:
                    event = pe.event
                    assert isinstance(event, ExogenousActionChoice)
                    if not self.__option_already_scheduled(current_time, event, pe.params):
                        if event.should_happen(evaluation_context, pe.params):
                            option = event.make_choice(evaluation_context, pe.params)
                            option_instance = EventOccurrence(current_time, option, pe.params)
                            heapq.heappush(self.__event_schedule, option_instance)
                        self.__already_processed_events.add(pe)

        for se in self.__schedulable_event_schedule:
            if (se.time_point == current_time
                and se not in self.__already_processed_events):
                ev_src = se.event
                if isinstance(ev_src, ExogenousAction):
                    event = ev_src
                    already_scheduled = se in self.__event_schedule
                else:
                    assert isinstance(ev_src, ExogenousActionChoice)
                    already_scheduled = self.__option_already_scheduled(current_time, ev_src, se.params)
                    if (not already_scheduled) and ev_src.should_happen(evaluation_context, se.params):
                        event = ev_src.make_choice(evaluation_context, se.params)
                    else:
                        event = None
                if not already_scheduled:
                    assert isinstance(event, ExogenousAction)
                    schedule_time = event.get_next_occurrence_time(evaluation_context, se.params)
                    if schedule_time is not None:
                        assert schedule_time >= current_time
                        new_ev = EventOccurrence(schedule_time, event, se.params)
                        heapq.heappush(self.__event_schedule, new_ev)
                # mark the fact that we sampled the occurrence distribution regardless of
                # whether or not it's been scheduled
                self.__already_processed_events.add(se)

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
            processed = self.__translate_event_instances_to_raw(self.__already_processed_events, evaluation_context)
            sched = self.__translate_event_instances_to_raw(self.__event_schedule, evaluation_context)

            # check whether new event instances are possible
            poss_events = self.__logics_engine.get_next_possible_ad_hoc_event_instances(scan_start, scan_time_limit,
                                                                                        processed)
            self.__translate_event_instances_from_raw(poss_events, self.__possible_event_schedule, evaluation_context)

            # check whether new event instances can be scheduled
            schedulable_events = self.__logics_engine.get_next_schedulable_event_instances(
                scan_start, scan_time_limit, sched, processed)
            self.__translate_event_instances_from_raw(schedulable_events, self.__schedulable_event_schedule,
                                                      evaluation_context)

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
            elif isinstance(act, ExogenousAction):
                # Note: for exogenous actions, the evaluation context is used that is
                # given as a parameter. This is usually the world's global context.
                #
                if len(act_seq) > 0:
                    fa = self.__logics_engine.progress(act_seq)
                    performed_actions.extend(act_seq)
                    failed.extend(fa)
                    act_seq.clear()

                action_name, args = act.generate_instance(evaluation_context, ai[1])
            elif isinstance(act, StochasticAction):
                if len(act_seq) > 0:
                    fa = self.__logics_engine.progress(act_seq)
                    performed_actions.extend(act_seq)
                    failed.extend(fa)
                    act_seq.clear()
                action_name, args = act.generate_outcome(ai[2], evaluation_context.lookup_entities(ai[1]))
            else:
                raise SALMAException("Unsupported action instance: {}".format(type(act)))
            act_seq.append((action_name, args))
        if len(act_seq) > 0:
            fa = self.__logics_engine.progress(act_seq)
            failed.extend(fa)
            performed_actions.extend(act_seq)
        # ignore failed events
        failed_intentional_actions = [fa for fa in failed if fa[0] not in self.__exogenous_actions]

        if logger.isEnabledFor(logging.DEBUG):
            logger.debug("  Progressed: %s, failed: %s", performed_actions, failed_intentional_actions)
        return performed_actions, failed_intentional_actions

    def __translate_event_instances_from_raw(self, raw_event_instances, schedule, evaluation_context):
        """
        Translates event tuples retrieved from the logics engine to tuples that refer to ExogenousAction entries.

        :param list[(int, str, tuple)] raw_event_instances: the event instances gathered by the engine
        :param list[EventOccurrence] schedule: the event schedule heap
            in which the translated event instances will be pushed.
        :param EvaluationContext evaluation_context: the evaluation context
        """
        for rev in raw_event_instances:
            ev_name = rev[1]
            if ev_name in self.__exogenous_actions:
                ea = self.__exogenous_actions[ev_name]
            elif ev_name in self.__exogenous_action_chooices:
                ea = self.__exogenous_action_chooices[ev_name]
            else:
                raise SALMAException("Unregistered exogenous action (choice): {}".format(ev_name))
            entity_params = evaluation_context.lookup_entities(rev[2])

            heapq.heappush(schedule, EventOccurrence(rev[0], ea, entity_params))

    # noinspection PyMethodMayBeStatic
    def __translate_event_instances_to_raw(self, schedule, evaluation_context):
        """
        Translates a generator for conversion of the current event schedule to a list of event tuples of form
        (time, action_name param_values).

        :param Iterable[EventOccurrence] schedule: the event schedule.
        :param EvaluationContext evaluation_context: the evaluation context
        :rtype: list[(int, str, tuple)]
        """
        result = []
        for entry in schedule:
            event = entry.event
            if isinstance(event, ExogenousAction):
                name = event.action_name
            else:
                assert isinstance(event, ExogenousActionChoice)
                name = event.choice_name
            result.append((entry.time_point, name, translate_entities(entry.params)))
        return result

    def check_exogenous_action_initialization(self):
        """
        Checks whether all registered exogenous actions are properly configured.
        :return: a list of tuples of form (exo_action, problems)
        :rtype: list[(ExogenousAction, list)]
        """
        problematic_exogenous_actions = []
        for exo_action in self.__exogenous_actions.values():
            assert isinstance(exo_action, ExogenousAction)
            if exo_action.config is None:
                problematic_exogenous_actions.append((exo_action, [("uninitialized", ())]))
            else:
                problems = exo_action.config.check()
                if len(problems) > 0:
                    problematic_exogenous_actions.append((exo_action, problems))
        return problematic_exogenous_actions

    def deactivate_all_events(self):
        """
        Equips all registered exogenous actions with distributions NEVER and DONT_OCCUR
        :return:
        """
        for exo_action in self.__exogenous_actions.values():
            assert isinstance(exo_action, ExogenousAction)
            if exo_action.scheduling_type in [ExogenousAction.SCHEDULABLE, ExogenousAction.CHOICE_OPTION,
                                              ExogenousAction.CAUSED]:
                exo_action.config.occurrence_distribution = NEVER
            else:
                exo_action.config.occurrence_distribution = DONT_OCCUR

    def check_exogenous_action_choice_initialization(self):
        """
        Checks whether all registered exogenous actions are properly configured.
        :return: a list of tuples of form (exo_action_choice, problems)
        :rtype: list[(ExogenousActionChoice, list)]
        """
        problematic = []
        for choice in self.__exogenous_action_chooices.values():
            assert isinstance(choice, ExogenousActionChoice)
            problems_for_choice = []
            if choice.occurrence_distribution is None:
                problems_for_choice.append(("no_occurrence_distribution_specified", ()))
            else:
                # only ad hoc is allowed for event choices
                if choice.occurrence_distribution.sort != "boolean":
                    problems_for_choice.append(("exogenous_action_choice_occurrence_not_boolean", ()))
            if choice.selection_strategy is None:
                problems_for_choice.append(("no_selection_strategy", ()))
            else:
                problems_for_choice.extend(choice.selection_strategy.check())
            if len(problems_for_choice) > 0:
                problematic.append((choice, problems_for_choice))
        return problematic

    def get_due_events(self, current_time):
        """
        Returns all events that are due at the current time (given as a parameter).

        :param int current_time: the current time step
        :return: a list of tuples of form (event, params)
        :rtype: list[(ExogenousAction, tuple)]
        """
        due_events = []
        while len(self.__event_schedule) > 0 and self.__event_schedule[0].time_point <= current_time:
            entry = heapq.heappop(self.__event_schedule)
            due_events.append((entry.event, entry.params))
        return due_events

    def get_next_time_checkpoint(self):
        """
        Returns the next time step that must be visited in the simulation because at that time
        a) an event is already scheduled, b) an ad-hoc becomes possible, or
          c) an event becomes schedulable.

        :rtype: int
        """
        t1 = self.__event_schedule[0].time_point if len(self.__event_schedule) > 0 else None
        t2 = None

        for ev in self.__possible_event_schedule:
            # it's enough to return the first unhandled entry since they're sorted by time
            if ev not in self.__already_processed_events:
                t2 = ev.time_point
                break

        t3 = None
        for ev in self.__schedulable_event_schedule:
            if ev not in self.__already_processed_events:
                t3 = ev.time_point
                break
        return min_robust([t1, t2, t3])

    def get_next_scheduled_event(self):
        """
        Returns the next scheduled event instance as a tuple of the form (time, (event, params)) or
        None if no event is currently scheduled.
        :rtype: EventOccurrence
        """
        if len(self.__event_schedule) > 0:
            return self.__event_schedule[0]
        else:
            return None
