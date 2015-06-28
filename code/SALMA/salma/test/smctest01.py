from logging import DEBUG, INFO
from unittest.case import TestCase
import unittest
from statsmodels.stats import proportion
from scipy.stats import norm
from scipy.stats import geom
import logging
from salma.model.experiment import SingleProcessExperimentRunner
from salma.statistics import SequentialProbabilityRatioTest
from salma.test.smctest_base_experiment import SMCTestBaseExperiment
import sys

MODULE_LOGGER_NAME = 'salma.model'
logging.basicConfig()
module_logger = logging.getLogger(MODULE_LOGGER_NAME)
module_logger.setLevel(INFO)
module_logger.info("bla")


def report_step(world, step=None, actions=None, **kwargs):
    """
    :type world: salma.model.world.World
    """
    print("Step {}: {}".format(step, actions))
    for r in world.getDomain("robot"):
        carrying = [it.id for it in world.getDomain("item") if world.getFluentValue("carrying", [r.id, it.id])]
        print("   {}({}, {}) carries {}".format(r.id,
                                                world.getFluentValue("xpos", [r.id]),
                                                world.getFluentValue("ypos", [r.id]),
                                                carrying))


class SMCTest01(TestCase):
    """
    Conducts experiments with a simple scenario whose distribution can be given in closed form.
    """

    def setUp(self):
        module_logger.info("setup")
        print("setup")
        num_robots = 3
        p_collision = 0.9
        p_drop = 0.2
        drop_delay_mean = 10
        drop_delay_std = 2
        time_limit = 25
        x_goal = 20
        x_goal2 = 20

        self.experiment = SMCTestBaseExperiment(num_robots, p_drop, drop_delay_mean, drop_delay_std,
                                                p_collision, time_limit, x_goal, x_goal2)
        self.experiment.setup_properties()
        self.experiment.initialize()
        # self.experiment.step_listeners.append(report_step)

    @unittest.skip("too long")
    def test_confidence_interval_estimation(self):
        runner = SingleProcessExperimentRunner()
        sample_length = 100
        samples = 1000
        alpha = 0.05
        method = "agresti_coull"
        estimation_tolerance = 0.1

        confidence_intervals = []
        all_successes = 0
        report_lines = []
        """:type : list[dict]"""
        for i in range(0, samples):
            _, res, trial_infos = runner.run_trials(self.experiment,
                                                    number_of_trials=sample_length,
                                                    step_listeners=[report_step])
            print(trial_infos)
            self.assertEqual(sample_length, len(res))
            self.assertEqual(sample_length, len(trial_infos))
            successes = sum(res)
            all_successes += successes
            ci_low, ci_up = proportion.proportion_confint(successes, len(res), alpha=alpha,
                                                          method=method)
            confidence_intervals.append((ci_low, ci_up))
            report_lines.append(dict(i=i+1, successes=successes, trials=len(res)))

            print("Run #{}: {} successes, CI: [{}..{}]".format(i + 1, successes, ci_low, ci_up))

        estimated_prob = all_successes / (samples * sample_length)

        # drop_delay_distrib = norm(self.experiment.drop_delay_mean, self.experiment.drop_delay_std)
        # real_prob = ((1.0 - self.experiment.p_drop * drop_delay_distrib.cdf(self.experiment.x_goal)) **
        #              self.experiment.num_robots)

        # {2: 0.02, 3: 0.05, 4: 0.1, 5: 0.3}
        fprobs = [(0.0, 0.2), (0.02, 0.2), (0.05, 0.2), (0.1, 0.2)]
        fail_prob_one_robot = 0
        for fp in fprobs:
            fail_prob_one_robot += fp[1] * geom.cdf(self.experiment.x_goal, fp[0])
        real_prob = (1.0 - fail_prob_one_robot) ** self.experiment.num_robots

        print("estimated probability: {}".format(estimated_prob))
        print("real probability: {}".format(real_prob))
        interval_hit = 0
        for cl, cu in confidence_intervals:
            if cl <= real_prob <= cu:
                interval_hit += 1
        interval_hit_ratio = interval_hit / len(confidence_intervals)
        print("interval hits: {} of {} = {} %".format(interval_hit, len(confidence_intervals),
                                                      interval_hit_ratio * 100.0))

        with open("smctest01_ci.csv", "w") as f:
            f.write("I;SUCCESSES;TRIALS\n")
            for line in report_lines:
                f.write("{i};{successes};{trials}\n".format(**line))

        # self.assertAlmostEqual(real_prob, estimated_prob, delta=estimation_tolerance)
        # self.assertTrue(interval_hit_ratio >= (1.0 - alpha))

    @unittest.skip("too long")
    def test_sprt(self):
        runner = SingleProcessExperimentRunner()

        p = 0.1

        report_lines = []
        """:type : list[dict]"""

        while p <= 0.35:
            sprt = SequentialProbabilityRatioTest(p - 0.05, min(1, p + 0.05), 0.05, 0.05)
            accepted_hypothesis, res, trial_infos = runner.run_trials(self.experiment,
                                                                      hypothesis_test=sprt,
                                                                      step_listeners=[report_step],
                                                                      max_trials=200)
            print("p={:.3}".format(p))
            print("Accepted Hypothesis: {}\nResults: {}\nTrial Infos: {}".format(accepted_hypothesis, res, trial_infos))
            trials = len(trial_infos)
            print("Trials: {}".format(trials))
            report_lines.append(dict(p=p, hyp=accepted_hypothesis, trials=trials))
            p += 0.05
        with open("smctest01.csv", "w") as f:
            f.write("P;HYP;TRIALS\n")
            for line in report_lines:
                f.write("{p:.3};{hyp};{trials}\n".format(**line))

    def test_calc_prob(self):
        fprobs = [(0.0, 0.25), (0.02, 0.25), (0.05, 0.25), (0.1, 0.25)]
        fail_prob_one_robot = 0
        for fp in fprobs:
            fail_prob_one_robot += fp[1] * geom.cdf(self.experiment.x_goal, fp[0])
        real_prob = (1.0 - fail_prob_one_robot) ** self.experiment.num_robots

        print("real probability: {}".format(real_prob))