from math import sqrt
from salma.SALMAException import SALMAException
from salma.model.core import Entity
from salma.model.world import World
from salma.test.world_test_base import BaseWorldTest


class EntityTest(BaseWorldTest):
    def setUp(self):
        super().setUp()
        world = World.instance()

        self.rob1 = self.create_right_moving_mobot('rob1')
        self.rob2 = self.create_right_moving_mobot('rob2')
        self.item3 = Entity("item3", "item")
        self.item1 = Entity("item1", "item")
        self.item2 = Entity("item2", "item")
        world.add(self.item3, self.rob2, self.item2, self.rob1, self.item1)
        world.initialize(False)
        self.initialize_robot("rob1", 10, 15, 1, -1, radius=5)
        self.initialize_robot("rob2", 100, 150, 0, 0, radius=8, active=False)
        world.set_fluent_value("carrying", [self.rob1.id, self.item1.id], True)
        world.set_fluent_value("carrying", [self.rob2.id, self.item2.id], True)

    def test_get_agent_properties(self):
        world = World.instance()
        x = self.rob1.get_own_fluent_value("xpos")
        print(x)
        self.assertEqual(x, 10)
        print("rob1: x = {}, y = {}, vx = {}, vy = {}\n".format(self.rob1.xpos, self.rob1.ypos, self.rob1.vx,
                                                                self.rob1.vy))
        self.assertEqual(self.rob1.xpos, 10)
        self.assertEqual(self.rob1.ypos, 15)
        self.assertEqual(self.rob1.vx, 1)
        self.assertEqual(self.rob1.vy, -1)

        print("rob2: x = {}, y = {}, vx = {}, vy = {}\n".format(self.rob2.xpos, self.rob2.ypos, self.rob2.vx,
                                                                self.rob2.vy))
        self.assertEqual(self.rob2.xpos, 100)
        self.assertEqual(self.rob2.ypos, 150)
        self.assertEqual(self.rob2.vx, 0)
        self.assertEqual(self.rob2.vy, 0)

        self.assertTrue(self.rob1.carrying(self.item1))
        self.assertTrue(self.rob2.carrying(self.item2))
        self.assertFalse(self.rob1.carrying(self.item2))
        self.assertFalse(self.rob2.carrying(self.item1))
        self.assertFalse(self.rob2.carrying("item1"))

        # derived fluents
        print("rob1.dist_from_origin = {}".format(self.rob1.dist_from_origin))
        self.assertEqual(self.rob1.dist_from_origin, sqrt(self.rob1.xpos ** 2 + self.rob1.ypos ** 2))
        self.assertTrue(self.rob1.moving)
        self.assertFalse(self.rob2.moving)
        # constants
        self.assertEqual(self.rob1.robot_radius, 5)
        self.assertEqual(self.rob2.robot_radius, 8)
        self.assertEqual(world.gravity, 9.81)

    def test_exception_if_undefined_feature(self):
        try:
            print(self.rob1.zpos)
            self.fail("Expected SALMAException")
        except SALMAException as se:
            print(se.message)

    def test_set_agent_properties(self):
        world = World.instance()
        self.rob1.xpos = 200
        self.rob1.ypos = 300
        self.rob2.xpos = 250
        self.rob2.ypos = 350

        self.assertEqual(world.get_fluent_value("xpos", ["rob1"]), 200)
        self.assertEqual(world.get_fluent_value("ypos", ["rob1"]), 300)
        self.assertEqual(world.get_fluent_value("xpos", ["rob2"]), 250)
        self.assertEqual(world.get_fluent_value("ypos", ["rob2"]), 350)

        self.rob1.robot_radius = 25
        self.rob2.robot_radius = 55
        self.assertEqual(world.getConstantValue("robot_radius", ["rob1"]), 25)
        self.assertEqual(world.getConstantValue("robot_radius", ["rob2"]), 55)

        world.gravity = 42.123
        self.assertEqual(world.getConstantValue("gravity", []), 42.123)

        self.assertFalse(world.get_fluent_value("carrying", ["rob1", "item2"]))
        self.rob1.set_carrying(self.item2, True)
        self.assertTrue(world.get_fluent_value("carrying", ["rob1", "item2"]))

        self.rob1.set_vx(42)
        self.assertTrue(world.get_fluent_value("vx", ["rob1"]), 42)

        world.set_gravity(4711)
        self.assertEqual(world.getConstantValue("gravity", []), 4711)

    def test_ordering(self):
        self.assertTrue(self.rob1 < self.rob2)
        self.assertFalse(self.rob2 < self.rob1)
        self.assertFalse(self.rob1 < self.rob1)
        self.assertTrue(self.item1 < self.rob1)
        self.assertTrue(self.item1 < Entity("item1b", "item"))
        self.assertTrue(self.item1 < Entity("item1", "itemb"))

        self.assertTrue(self.rob2 > self.rob1)
        self.assertFalse(self.rob1 > self.rob2)
        self.assertFalse(self.rob1 > self.rob1)

        self.assertTrue(self.rob2 >= self.rob1)
        self.assertFalse(self.rob1 >= self.rob2)
        self.assertTrue(self.rob1 >= self.rob1)

        self.assertTrue(self.rob1 <= self.rob2)
        self.assertFalse(self.rob2 <= self.rob1)
        self.assertTrue(self.rob1 <= self.rob1)

        self.assertTrue(self.rob1 == self.rob1)
        self.assertTrue(self.rob1 != self.rob2)
        self.assertFalse(self.rob1 == self.rob2)

        world = World.instance()
        self.assertListEqual(sorted(world.getDomain("robot") | world.getDomain("item")),
                             [self.item1, self.item2, self.item3, self.rob1, self.rob2])
