
class Experiment(object):
    """
    Contains the complete setup of an experiment including agent processes, initial situation generator,
    probability distributions, and properties to evaluate.
    """

    def __init__(self):
        pass

    def initialize(self):
        pass

    def runExperiment(self, check_verdict=True, maxSteps=None, maxRealTime=None, maxWorldTime=None, stepListeners=[]):
        """
        Runs the experiment that has been set up until a) a conclusive verdict can be determined,
        b) the world has finished, c) the given step or time maximum is reached, or d) at least one of
        the given step listener functions returns False.

        If check_verdict is False then the registered properties are not evaluated and the verdict remains NONDET.

        :param bool check_verdict: whether properties are evaluated. default=True
        :param int maxSteps: maximum number of steps
        :param float maxRealTime: maximum real time
        :param int maxWorldTime: maximum world time
        :param list stepListeners: step listener functions with siugnature (step_num, deltaT, actions, toplevel_results)
        :rtype: (int, dict[str, object])
        """
        step_num = 0
        verdict = NONDET
        self.__already_achieved_goals = set()
        failedRegularActions = []
        c1 = c2 = time.clock()
        finish_reason = None
        failed_invariants = set()
        failed_sustain_goals = set()
        failure_stack = []
        # : :type: dict[str, list[int]]
        scheduled_keys = dict()
        time_out = False

        while (not self.is_finished()) and (not check_verdict or verdict == NONDET):
            # self.__finished, overall_verdict, toplevel_results, scheduled_results, actions, []
            (verdict, _, toplevel_results, scheduled_results, scheduled_keys, actions, failedRegularActions,
             failed_invariants, failed_sustain_goals, failure_stack) = self.step(evaluate_properties=check_verdict)
            c2 = time.clock()
            step_num += 1
            deltaT = c2 - c1
            should_continue = True
            break_reason = None
            for sl in stepListeners:
                continue_from_listener, break_reason_from_listener = sl(self,
                                                                        verdict=verdict,
                                                                        step=step_num, deltaT=deltaT,
                                                                        actions=actions,
                                                                        failedActions=failedRegularActions,
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
            if failedRegularActions is not None and len(failedRegularActions) > 0:
                finish_reason = "failed_actions"
                verdict = CANCEL
                break
            if maxSteps != None and step_num >= maxSteps:
                finish_reason = "max_steps"
                time_out = True
                break
            if maxRealTime != None and datetime.timedelta(seconds=deltaT) >= maxRealTime:
                finish_reason = "max_real_time"
                time_out = True
                break
            if maxWorldTime != None:
                t = self.getFluentValue('time', [])
                if t >= maxWorldTime:
                    finish_reason = "max_world_time"
                    time_out = True
                    break
        if finish_reason is None and verdict != NONDET:
            finish_reason = "verdict_found"
        if self.is_finished():
            finish_reason = "world_finished"
        if verdict == NONDET:
            if check_verdict is False:
                verdict = OK if self.is_finished() else NOT_OK
            # if no achieve goal was given then having finished or "surviving" until the time limit means success!
            # However, this only holds if no invariants are pending. Otherwise, we will return NONDET
            else:
                if ((self.is_finished() or time_out is True) and
                            len(self.__achieve_goals) == 0 and
                            len(self.__achieve_and_sustain_goals) == 0 and
                            len(scheduled_keys) == 0):
                    verdict = OK

        duration = datetime.timedelta(seconds=c2 - c1)
        worldTime = self.getFluentValue('time', [])
        return (verdict,
                {'steps': step_num,
                 'time': duration,
                 'worldTime': worldTime,
                 'failedActions': failedRegularActions,
                 "finish_reason": finish_reason,
                 "failed_invariants": failed_invariants,
                 "failed_sustain_goals": failed_sustain_goals,
                 "achieved_goals": self.__already_achieved_goals,
                 "failure_stack": failure_stack,
                 "scheduled_keys": scheduled_keys})

    def runUntilFinished(self, maxSteps=None, maxRealTime=None, maxWorldTime=None, stepListeners=[]):
        """
        Repeatedly runs World.step() until either the world's finished flag becomes true or
        either the step or time limit is reached. The properties are not evaluated.

        :param int maxSteps: maximum number of steps
        :param float maxRealTime: maximum real time
        :param int maxWorldTime: maximum world time
        :param list stepListeners: step listener functions with siugnature (step_num, deltaT, actions, toplevel_results)
        :rtype: (int, dict[str, object])
        """
        verdict, results = self.runExperiment(check_verdict=False, maxSteps=maxSteps, maxRealTime=maxRealTime,
                                              maxWorldTime=maxWorldTime, stepListeners=stepListeners)
        if verdict != CANCEL:
            verdict = OK if self.is_finished() else NOT_OK
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
            self.reset()
            World.logic_engine().restoreState(current_state)

            verdict, res = self.runExperiment(**kwargs)
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
                # moduleLogger.info("Trial #{} --> {}".format(trial_number, verdict))
                self.log_info(
                    "Trial #{} --> {}, steps = {}, time = {}".format(trial_number, verdict, res["steps"], res["time"]))
                # print("Trial #{} --> {}\n   Info: {}".format(trial_number, verdict, res))
            trial_number += 1

            if hypothesis_test is not None:
                accepted_hypothesis = hypothesis_test.check_hypothesis_accepted(conclusive_trial_count, failures)
                should_continue = accepted_hypothesis is None
            else:
                should_continue = conclusive_trial_count < number_of_repetitions

        self.reset()
        World.logic_engine().restoreState(current_state)
        return accepted_hypothesis, results, trial_infos