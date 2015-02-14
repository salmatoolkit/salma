import logging
from salma.model.agent import Agent
from salma.model.core import Entity
from salma.model.distributions import ConstantDistribution, OptionalDistribution, ExponentialDistribution, \
    BernoulliDistribution, Never, NormalDistribution
from salma.model.evaluationcontext import EvaluationContext
from salma.model.experiment import Experiment
from salma.model.procedure import While, Act, Wait, Procedure
from salma.model.process import TriggeredProcess


class MyExperiment(Experiment):
    def __init__(self):
        super().__init__("ecl-test/domaindesc.ecl")

    def augment_world_context(self):
        self.world.register_clp_function("robotLeftFrom")

    # def create_entities(self):
    # proc = Procedure("main", [], [
    #         Act("move_right", [Entity.SELF])
    #     ])
    #     p = TriggeredProcess(proc, EvaluationContext.PYTHON_EXPRESSION, "not moving(self)", [])
    #     self.world.addAgent(Agent("rob1", "robot", [p]))

    def create_entities(self):
        w = While("robotLeftFrom",
                  [Entity.SELF, 100],
                  [
                      Act("move_right", [Entity.SELF]),
                      Wait("not moving(self)")
                  ])

        agent = Agent("rob1", "robot", Procedure("main", [], w))
        self.world.addAgent(agent)

    def initialize_robot(self, robot_id, x, y, vx, vy):
        self.world.setFluentValue("xpos", [robot_id], x)
        self.world.setFluentValue("ypos", [robot_id], y)
        self.world.setFluentValue("vx", [robot_id], vx)
        self.world.setFluentValue("vy", [robot_id], vy)
        self.world.setConstantValue("robot_radius", [robot_id], 1)
        self.world.setFluentValue("active", [robot_id], True)
        self.world.setFluentValue("partner", [robot_id], None)
        items = self.world.getDomain('item')
        for i in items:
            self.world.setFluentValue('carrying', [robot_id, i.id], False)

    def setup_distributions(self):
        self.world.deactivate_info_transfer()
        finish_step = self.world.get_exogenous_action("finish_step")
        finish_step.config.occurrence_distribution = ConstantDistribution("integer", 5)
        accidental_drop = self.world.get_exogenous_action("accidental_drop")
        accidental_drop.config.occurrence_distribution = Never()
        collision = self.world.get_exogenous_action("collision")
        collision.config.occurrence_distribution = BernoulliDistribution(1.0)
        collision.config.set_param_distribution("severity", NormalDistribution("integer", 10.0, 1.0))

    def create_initial_situation(self):
        self.world.setConstantValue("gravity", [], 9.81)
        self.initialize_robot("rob1", 10, 10, 0, 0)


if __name__ == '__main__':
    ch = logging.StreamHandler()
    logger2 = logging.getLogger('salma.model')
    logger2.setLevel(logging.DEBUG)
    logger2.addHandler(ch)
    e = MyExperiment()
    e.initialize()
    verdict, info = e.run_until_finished()
    print("Verdict: {}  after {} steps".format(verdict, info['steps']))
    e.world.printState()
    print(info)
    print(e.world.is_finished())






