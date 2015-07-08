from unittest.case import TestCase
from salma.psl.internaldsl import *

__author__ = 'Christian'


class PSLInternalDSLTest(TestCase):
    def test_formula_1(self):
        f = Implies(Occur("grab", ["rob1", "?"]),
                    Eventually(100, "xpos(r) > 20"))
        print(str(f))
        self.assertEqual(str(f), "implies(occur(grab(rob1, ?)), eventually(100, xpos(r) > 20))")

    def test_formula_2(self):
        f = Forall("r", "robot",
                   Implies(
                       Exists("r2", "robot", "xpos(r2) > 200"),
                       Let("xmax", "max_robot_position",
                           Until(100, "moving(r)", "xpos(r) > xmax"))))
        print(str(f))
        self.assertEqual(str(f),
                         "forall(r:robot, implies(exists(r2:robot, xpos(r2) > 200), "
                         "let(xmax: max_robot_position, until(100, moving(r), xpos(r) > xmax))))")

    def test_formula_3(self):
        f = Until(40, True, "xpos(r) > 0")
        print(f)
        self.assertEqual(str(f), "until(40, true, xpos(r) > 0)")

    def test_formula_4(self):
        f = And("xpos(rob1) > 10",
                "ypos(rob2) < 100",
                Or(
                    "xpos(rob3) < 1000",
                    Not("active(rob3)")))
        print(f)
        print(repr(f))
        self.assertEqual(str(f), "and(xpos(rob1) > 10, ypos(rob2) < 100, or(xpos(rob3) < 1000, not(active(rob3))))")

    def test_formula_5(self):
        f = Let("x1", "(xpos(rob1) + 4) * 2",
                Eventually("{time_limit}", "xpos(rob1) > {xmax}"))
        print(f)
        print(f.get_free_variables())