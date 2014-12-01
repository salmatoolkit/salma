import logging
from salma.constants import *
from salma.model.propertycollection import PropertyCollection
from salma.model.world import World
from salma.mathutils import min_robust
import time
import datetime

MODULE_LOGGER_NAME = 'salma.model'
moduleLogger = logging.getLogger(MODULE_LOGGER_NAME)

DEFAULT_MAX_TIME_DELTA_PER_STEP = 100000


class Experiment(object):
    """
    Contains the complete setup of an experiment including agent processes, initial situation generator,
    probability distributions, and properties to evaluate.
    """

    def __init__(self, world):
        """
        Creates an experiment instance based on the given world.

        :param World world: the world this experiment uses
        """
        # : :type: World
        self.__world = world
        #: :type: PropertyCollection
        self.__property_collection = PropertyCollection(World.logic_engine())

    # --- PROPERTIES
    @property
    def property_collection(self):
        """
        Handles all properties registered for this world.
        :rtype: PropertyCollection
        """
        return self.__property_collection

    # ---

    def initialize(self):
        pass

    def run_experiment(self, check_verdict=True, max_steps=None, max_real_time=None, max_world_time=None,
                       max_time_delta_per_step=None, step_listeners=None):
        """
        Runs the experiment that has been set up until a) a conclusive verdict can be determined,
        b) the world has finished, c) the given step or time maximum is reached, or d) at least one of
        the given step listener functions returns False.

        If check_verdict is False then the registered properties are not evaluated and the verdict remains NONDET.

        :param bool check_verdict: whether properties are evaluated. default=True
        :param int max_steps: maximum number of steps
        :param float max_real_time: maximum real time
        :param int max_world_time: maximum world time
        :param int max_time_delta_per_step: the maximum time interval that a step is allowed to span
        :param list step_listeners: step listener functions with siugnature (step_num, delta_t, actions, toplevel_results)
        :return: (verdict, result-map)
        :rtype: (int, dict[str, object])
        """
        if not step_listeners:
            step_listeners = []
        step_num = 0
        verdict = NONDET if check_verdict else None
        self.__property_collection.reset()
        failed_regular_actions = []
        c1 = c2 = time.clock()
        finish_reason = None
        failed_invariants = set()
        failed_sustain_goals = set()
        failure_stack = []
        # : :type: dict[str, list[int]]
        scheduled_keys = dict()
        time_out = False
        max_delta = max_time_delta_per_step

        # only use default maximum time delta if no other bound was specified
        if max_time_delta_per_step is None and max_world_time is None:
            max_delta = DEFAULT_MAX_TIME_DELTA_PER_STEP

        while (not self.__world.is_finished()) and (not check_verdict or verdict == NONDET):
            current_time = self.__world.getTime()
            #: :type: int
            time_limit = min_robust([max_world_time, current_time + max_delta])
            (_, toplevel_results, scheduled_results, scheduled_keys, actions, failed_regular_actions,
             failure_stack) = self.__world.step(
                time_limit, evaluate_properties=check_verdict)

            if check_verdict:
                verdict, failed_invariants, failed_sustain_goals = self.property_collection.arbitrate_verdict(
                    toplevel_results,
                    scheduled_results,
                    scheduled_keys)
            c2 = time.clock()
            step_num += 1
            delta_t = c2 - c1
            should_continue = True
            break_reason = None
            for sl in step_listeners:
                continue_from_listener, break_reason_from_listener = sl(self.__world,
                                                                        verdict=verdict,
                                                                        step=step_num, deltaT=delta_t,
                                                                        actions=actions,
                                                                        failedActions=failed_regular_actions,
                                                                        toplevel_results=toplevel_results,
                                                                        scheduled_results=scheduled_results,
                                                                        pending_properties=scheduled_keys)
                should_continue &= continue_from_listener
                if break_reason is None and not continue_from_listener:
                    break_reason = break_reason_from_listener
            # note that reason of step listener gets precedence over other reasons
            if not should_continue:
                finish_reason = break_reason
                verdict = CANCEL
                break
            if failed_regular_actions is not None and len(failed_regular_actions) > 0:
                finish_reason = "failed_actions"
                verdict = CANCEL
                break
            if max_steps is not None and step_num >= max_steps:
                finish_reason = "max_steps"
                time_out = True
                break
            if max_real_time is not None and datetime.timedelta(seconds=delta_t) >= max_real_time:
                finish_reason = "max_real_time"
                time_out = True
                break
            if max_world_time is not None:
                if current_time >= max_world_time:
                    finish_reason = "max_world_time"
                    time_out = True
                    break
        if finish_reason is None and verdict != NONDET:
            finish_reason = "verdict_found"
        if self.__world.is_finished():
            finish_reason = "world_finished"
        if verdict == NONDET:
            if check_verdict is False:
                verdict = OK if self.__world.is_finished() else NOT_OK
            # if no achieve goal was given then having finished or "surviving" until the time limit means success!
            # However, this only holds if no invariants are pending. Otherwise, we will return NONDET
            else:
                if ((self.__world.is_finished() or time_out is True) and
                            len(self.property_collection.achieve_goals) == 0 and
                            len(self.property_collection.achieve_and_sustain_goals) == 0 and
                            len(scheduled_keys) == 0):
                    verdict = OK

        duration = datetime.timedelta(seconds=c2 - c1)
        world_time = self.__world.getTime()
        return (verdict,
                {'steps': step_num,
                 'time': duration,
                 'worldTime': world_time,
                 'failedActions': failed_regular_actions,
                 "finish_reason": finish_reason,
                 "failed_invariants": failed_invariants,
                 "failed_sustain_goals": failed_sustain_goals,
                 "achieved_goals": self.__property_collection.already_achieved_goals,
                 "failure_stack": failure_stack,
                 "scheduled_keys": scheduled_keys})

    def run_until_finished(self, max_steps=None, max_real_time=None, max_world_time=None, step_listeners=None):
        """
        Repeatedly runs World.step() until either the world's finished flag becomes true or
        either the step or time limit is reached. The properties are not evaluated.

        :param int max_steps: maximum number of steps
        :param float max_real_time: maximum real time
        :param int max_world_time: maximum world time
        :param list step_listeners: step listener functions with siugnature (step_num, deltaT, actions, toplevel_results)
        :rtype: (int, dict[str, object])
        """
        if not step_listeners:
            step_listeners = []
        verdict, results = self.run_experiment(check_verdict=False, max_steps=max_steps, max_real_time=max_real_time,
                                               max_world_time=max_world_time, step_listeners=step_listeners)
        if verdict != CANCEL:
            verdict = OK if self.__world.is_finished() else NOT_OK
        return verdict, results

    def run_repetitions(self, number_of_repetitions=100, max_retrials=3, hypothesis_test=None, **kwargs):
        """
        Runs repetitions of the configured experiment. If an hypothesis test object is given then the acceptance
        of this hypothesis test is
        :param int number_of_repetitions: fixed number of repetitions if no hypothesis test is given
        :param int max_retrials: maximum number of retrials
        :param HypothesisTest hypothesis_test: the (sequential) hypothesis test to conduct
        :return:
        """
        # save state
        current_state = [fv for fv in World.logic_engine().getCurrentState() if fv.fluentName != "domain"]
        results = []  # list of True/False
        trial_infos = []
        retrial = 0
        conclusive_trial_count = 0
        trial_number = 1
        should_continue = True
        accepted_hypothesis = None
        successes, failures = 0, 0
        while should_continue:
            self.__world.reset()
            self.__world.restore_state(current_state)

            verdict, res = self.run_experiment(**kwargs)
            trial_infos.append(res)
            if verdict == NONDET or verdict == CANCEL:
                if verdict == CANCEL:
                    moduleLogger.warn("Trail #{} was canceled! Reason: {}".format(trial_number, res["finish_reason"]))
                if verdict == NONDET:
                    moduleLogger.warn("Received non-conclusive result for trial #{}!".format(trial_number))
                if retrial < max_retrials:
                    retrial += 1
                    moduleLogger.warn("Starting retrial #".format(retrial))
                else:
                    moduleLogger.warn("Maximum number of retrials reached ({}) --> giving up!".format(retrial))
                    break
            else:
                retrial = 0
                results.append(verdict == OK)
                if verdict == OK:
                    successes += 1
                else:
                    failures += 1
                conclusive_trial_count += 1
                moduleLogger.info(
                    "Trial #{} --> {}, steps = {}, time = {}".format(trial_number, verdict, res["steps"], res["time"]))
            trial_number += 1

            if hypothesis_test is not None:
                accepted_hypothesis = hypothesis_test.check_hypothesis_accepted(conclusive_trial_count, failures)
                should_continue = accepted_hypothesis is None
            else:
                should_continue = conclusive_trial_count < number_of_repetitions

        self.__world.reset()
        self.__world.restore_state(current_state)
        return accepted_hypothesis, results, trial_infos