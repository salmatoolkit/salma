from unittest.case import TestCase

from statsmodels.stats import proportion
from scipy.stats import norm

from salma.model.experiment import SingleProcessExperimentRunner
from salma.test.smctest_base_experiment import SMCTestBaseExperiment


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

    def test_confidence_interval_estimation(self):
        runner = SingleProcessExperimentRunner()
        sample_length = 20
        samples = 20
        alpha = 0.05
        method = "agresti_coull"
        estimation_tolerance = 0.1

        confidence_intervals = []
        all_successes = 0
        for i in range(0, samples):
            _, res, trial_infos = runner.run_repetitions(self.experiment,
                                                         number_of_repetitions=sample_length,
                                                         step_listeners=[report_step])
            print(trial_infos)
            self.assertEqual(sample_length, len(res))
            self.assertEqual(sample_length, len(trial_infos))
            successes = sum(res)
            all_successes += successes
            ci_low, ci_up = proportion.proportion_confint(successes, len(res), alpha=alpha,
                                                          method=method)
            confidence_intervals.append((ci_low, ci_up))

            print("Run #{}: {} successes, CI: [{}..{}]".format(i + 1, successes, ci_low, ci_up))

        estimated_prob = all_successes / (samples * sample_length)

        drop_delay_distrib = norm(self.experiment.drop_delay_mean, self.experiment.drop_delay_std)
        real_prob = ((1.0 - self.experiment.p_drop * drop_delay_distrib.cdf(self.experiment.x_goal)) **
                     self.experiment.num_robots)
        print("estimated probability: {}".format(estimated_prob))
        print("real probability: {}".format(real_prob))
        interval_hit = 0
        for cl, cu in confidence_intervals:
            if cl <= real_prob <= cu:
                interval_hit += 1
        interval_hit_ratio = interval_hit / len(confidence_intervals) * 100.0
        print("interval hits: {} of {} = {} %".format(interval_hit, len(confidence_intervals),
                                                      interval_hit_ratio))
        self.assertAlmostEqual(real_prob, estimated_prob, delta=estimation_tolerance)
        self.assertTrue(interval_hit_ratio >= (1.0 - alpha))

