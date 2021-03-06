import logging
from salma.SALMAException import SALMAException
from salma.constants import *
from salma.engine import EclipseCLPEngine
from salma.model.actions import DeterministicAction
from salma.model.propertycollection import PropertyCollection
from salma.model.world import World
from salma.mathutils import min_robust
import time
import datetime
from collections.abc import Callable
from salma.smcutils import verdict_and

logger = logging.getLogger(__name__)

DEFAULT_MAX_TIME_DELTA_PER_STEP = 100000
DEFAULT_MAX_TRIALS = 500


class Experiment(object):
    """
    Contains the complete setup of an experiment including agent processes, initial situation generator,
    probability distributions, and properties to evaluate.
    """

    def __init__(self, world, procedure_defs=None):
        """
        Creates an experiment instance based on the given world.

        :param World|str world: the world this experiment uses
        :param str|tuple[str]|list[str] procedure_defs: a path or a couple of paths to Eclipse procedure definitions.
        """
        # : :type: World
        if isinstance(world, World):
            self.__world = world
            self.__world_declaration_path = None
        elif isinstance(world, str):
            self.__world = None
            self.__world_declaration_path = world
        self.__procedure_defs_path = procedure_defs
        if self.__world is None:
            if self.world_declaration_path is None:
                raise SALMAException("Neither world not world declaration path given for experiment.")
            World.set_logic_engine(EclipseCLPEngine(self.world_declaration_path, self.procedure_defs_path))

        #: :type: PropertyCollection
        self.__property_collection = PropertyCollection(World.logic_engine())
        #: :type: list[call
        self.__step_listeners = []

    # --- PROPERTIES
    @property
    def property_collection(self):
        """
        Handles all properties registered for this world.
        :rtype: PropertyCollection
        """
        return self.__property_collection

    @property
    def world(self):
        """
        Returns the world of this experiment.
        :rtype: World
        """
        return self.__world

    @property
    def world_declaration_path(self):
        """
        :rtype: str
        """
        return self.__world_declaration_path

    @property
    def procedure_defs_path(self):
        """
        :rtype: str
        """
        return self.__procedure_defs_path

    @property
    def step_listeners(self):
        """
        The registered step listeners.
        :rtype: list[Callable]
        """
        return self.__step_listeners

    # ---

    def initialize(self):
        if self.world is None:
            World.create_new_world(erase_properties=False)
            self.__world = World.instance()
            self.world.load_declarations()
            self.augment_world_context()

        self.create_entities()
        self.world.initialize()
        self.setup_distributions()
        self.create_initial_situation()

    def reset(self):
        self.__world = None
        self.initialize()

    def augment_world_context(self):
        pass

    def create_entities(self):
        pass

    def setup_distributions(self):
        pass

    def create_initial_situation(self):
        pass

    def setup_properties(self):
        pass

    def before_run(self, **kwargs):
        pass

    def after_run(self, verdict, **kwargs):
        pass

    def assure_consistent_initilization(self):
        problematic_fluents, problematic_constants = self.__world.check_fluent_initialization()
        failed = False
        if problematic_fluents is not None and len(problematic_fluents) > 0:
            logger.error("Fluent initialization problems:\n{}".format(problematic_fluents))
            failed = True
        if problematic_constants is not None and len(problematic_constants) > 0:
            logger.error("Constant initialization problems:\n{}".format(problematic_constants))
            failed = True
        (problematic_stochastic_actions, problematic_exogenous_actions,
         problematic_exogenous_action_choices) = self.__world.check_action_initialization()

        if problematic_stochastic_actions is not None and len(problematic_stochastic_actions) > 0:
            logger.error("Stochastic Action initialization problems:\n{}".format(problematic_stochastic_actions))
            failed = True
        if problematic_exogenous_actions is not None and len(problematic_exogenous_actions) > 0:
            logger.error("Exogenous Action initialization problems:\n{}".format(problematic_exogenous_actions))
            failed = True
        if problematic_exogenous_action_choices is not None and len(problematic_exogenous_action_choices) > 0:
            logger.error(
                "Exogenous Action Choice initialization problems:\n{}".format(problematic_exogenous_action_choices))
            failed = True
        if failed:
            raise SALMAException("Initialisation problems.")

    def run(self, **kwargs):
        """
        Runs the experiment that has been set up until a) a conclusive verdict can be determined,
        b) the world has finished, c) the given step or time maximum is reached, or d) at least one of
        the given step listener functions returns False.

        If check_verdict is False then the registered properties are not evaluated and the verdict remains NONDET.

        :parameter bool check_verdict: whether properties are evaluated. default=True
        :parameter int max_steps: maximum number of steps
        :parameter float max_real_time: maximum real time
        :parameter int max_world_time: maximum world time
        :parameter int max_time_delta_per_step: the maximum time interval that a step is allowed to span
        :return: (verdict, result-map)
        :rtype: (int, dict[str, object])
        """
        if len(self.property_collection.properties) > 0:
            check_verdict = kwargs.get("check_verdict", True)
        else:
            check_verdict = False

        max_steps = kwargs.get("max_steps", None)
        max_real_time = kwargs.get("max_real_time", None)
        max_world_time = kwargs.get("max_world_time", None)
        max_time_delta_per_step = kwargs.get("max_time_delta_per_step", None)
        check_initialization = kwargs.get("check_initialization", True)

        if check_initialization:
            self.assure_consistent_initilization()

        step_num = 0
        verdict = NONDET if check_verdict else None
        verdicts_by_step_listeners = []
        self.__property_collection.reset()
        failed_critical_actions = []
        c1 = c2 = time.clock()
        finish_reason = None
        failed_invariants = set()
        failed_sustain_goals = set()
        failure_stack = []
        # : :type: dict[str, list[int]]
        scheduled_keys = dict()
        time_out = False
        # ---- BEFORE -----------
        self.before_run(**kwargs)

        while (not self.__world.is_finished()) and ((not check_verdict and verdict is None) or verdict == NONDET):
            current_time = self.__world.getTime()
            # only use default maximum time delta if no other bound was specified
            if max_time_delta_per_step is not None:
                time_limit = min_robust([max_world_time, current_time + max_time_delta_per_step])
            else:
                if max_world_time is not None:
                    time_limit = max_world_time
                else:
                    time_limit = current_time + DEFAULT_MAX_TIME_DELTA_PER_STEP

            (_, toplevel_results, scheduled_results, scheduled_keys, actions, all_failed_actions,
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
            verdicts_by_step_listeners.clear()
            for sl in self.step_listeners:
                sl_res = sl(self.__world,
                            verdict=verdict,
                            step=step_num, deltaT=delta_t,
                            actions=actions,
                            failedActions=all_failed_actions,
                            toplevel_results=toplevel_results,
                            scheduled_results=scheduled_results,
                            pending_properties=scheduled_keys)
                if sl_res is None:
                    continue_from_listener = True
                    break_reason_from_listener = None
                elif isinstance(sl_res, str):
                    continue_from_listener = False
                    break_reason_from_listener = sl_res
                elif isinstance(sl_res, bool):
                    continue_from_listener = sl_res
                    break_reason_from_listener = (
                        None if continue_from_listener else "cancelled by step listener {}".format(
                            sl.__name__))
                elif isinstance(sl_res, tuple) and len(sl_res) == 2 and isinstance(sl_res[0], bool):
                    continue_from_listener = sl_res[0]
                    break_reason_from_listener = str(sl_res[1])
                elif sl_res in [OK, NOT_OK, NONDET]:
                    verdict = verdict_and(verdict, sl_res)
                    continue_from_listener = True  # the loop will be left due to while condition
                    break_reason_from_listener = None
                    verdicts_by_step_listeners.append((sl_res, sl))
                else:
                    raise SALMAException("Invalid return value from step listener {}: {}".format(sl, sl_res))
                should_continue &= continue_from_listener
                if break_reason is None:
                    break_reason = break_reason_from_listener
            # note that reason of step listener gets precedence over other reasons
            if not should_continue:
                finish_reason = break_reason
                verdict = CANCEL
                break
            if all_failed_actions is not None and len(all_failed_actions) > 0:
                for fatuple in all_failed_actions:
                    fa = self.world.getAction(fatuple[0])
                    if isinstance(fa, DeterministicAction):
                        if fa.break_on_failure:
                            finish_reason = "failed_actions"
                            verdict = CANCEL
                            failed_critical_actions.append(fatuple)
                if verdict == CANCEL:
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
        # --------------- END OF MAIN LOOP
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
        details = {'steps': step_num,
                   'time': duration,
                   'worldTime': world_time,
                   'failedActions': failed_critical_actions,
                   "finish_reason": finish_reason,
                   "failed_invariants": failed_invariants,
                   "failed_sustain_goals": failed_sustain_goals,
                   "achieved_goals": self.__property_collection.already_achieved_goals,
                   "failure_stack": failure_stack,
                   "scheduled_keys": scheduled_keys,
                   "verdicts_by_step_listeners": verdicts_by_step_listeners}
        # ---- AFTER -----------
        self.after_run(verdict, **details)
        return verdict, details

    def run_until_finished(self, **kwargs):
        """
        Repeatedly runs World.step() until either the world's finished flag becomes true or
        either the step or time limit is reached. The properties are not evaluated.

        :parameter bool check_verdict: whether properties are evaluated. default=True
        :parameter int max_steps: maximum number of steps
        :parameter float max_real_time: maximum real time
        :parameter int max_world_time: maximum world time
        :parameter int max_time_delta_per_step: the maximum time interval that a step is allowed to span
        :return: (verdict, result-map)
        :rtype: (int, dict[str, object])
        """
        check_initialization = kwargs.get("check_initialization", True)
        if check_initialization:
            self.assure_consistent_initilization()
        kwargs["check_verdict"] = False
        verdict, results = self.run(**kwargs)
        if verdict != CANCEL:
            verdict = OK if self.__world.is_finished() else NOT_OK
        return verdict, results


class ExperimentRunner(object):
    def run_trials(self, experiment, number_of_repetitions=100, hypothesis_test=None, **kwargs):
        """
        Runs repetitions of the configured experiment. If an hypothesis test object is given then the acceptance
        of this hypothesis test is calculated.

        :param Experiment experiment: the experiment to run.
        :param int number_of_repetitions: fixed number of repetitions if no hypothesis test is given
        :param HypothesisTest hypothesis_test: the (sequential) hypothesis test to conduct
        :return:
        """
        raise NotImplementedError()


class SingleProcessExperimentRunner(ExperimentRunner):
    def run_trials(self, experiment, number_of_trials=None, hypothesis_test=None, **kwargs):
        """
        Runs repetitions of the configured experiment. If an hypothesis test object is given then the acceptance
        of this hypothesis test is calculated.

        :param Experiment experiment: the experiment to run.
        :param int number_of_trials: fixed number of repetitions if no hypothesis test is given
        :param HypothesisTest hypothesis_test: the (sequential) hypothesis test to conduct
        :return:
        """
        logger.info("Starting trials")
        check_initialization = kwargs.get("check_initialization", True)
        max_retrials = kwargs.get("max_retrials", 3)
        max_trials = kwargs.get("max_trials", DEFAULT_MAX_TRIALS)
        if number_of_trials is None and hypothesis_test is None:
            raise SALMAException("Neither a number of trials nor a hypothesis test was specified.")
        if check_initialization:
            experiment.assure_consistent_initilization()
        results = []  # list of True/False
        trial_infos = []
        retrial = 0
        conclusive_trial_count = 0
        trial_number = 1
        should_continue = True
        accepted_hypothesis = None
        successes, failures = 0, 0
        while should_continue:
            experiment.reset()

            verdict, res = experiment.run(**kwargs)
            trial_infos.append(res)
            if verdict == NONDET or verdict == CANCEL:
                if verdict == CANCEL:
                    logger.warn("Trail #{} was canceled! Reason: {}".format(trial_number, res["finish_reason"]))
                if verdict == NONDET:
                    logger.warn("Received non-conclusive result for trial #{}!".format(trial_number))
                if retrial < max_retrials:
                    retrial += 1
                    logger.warn("Starting retrial #".format(retrial))
                else:
                    logger.warn("Maximum number of retrials reached ({}) --> giving up!".format(retrial))
                    break
            else:
                retrial = 0
                results.append(verdict == OK)
                if verdict == OK:
                    successes += 1
                else:
                    failures += 1
                conclusive_trial_count += 1
                logger.info(
                    "Trial #{} --> {}, steps = {}, time = {}".format(trial_number, verdict, res["steps"], res["time"]))
            trial_number += 1

            if hypothesis_test is not None:
                accepted_hypothesis = hypothesis_test.get_accepted_hypothesis(conclusive_trial_count, failures)
                should_continue = accepted_hypothesis is None and trial_number < max_trials
            else:
                should_continue = conclusive_trial_count < number_of_trials

        return accepted_hypothesis, results, trial_infos
