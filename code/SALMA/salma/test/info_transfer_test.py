from salma.SALMAException import SALMAException
from salma.engine import EclipseCLPEngine
import logging
from salma.model.infotransfer import Channel, Sensor, RemoteSensor

__author__ = 'Christian'

import unittest

from salma.model.world import World


class InfoTransferTest(unittest.TestCase):

    @classmethod
    def setUpClass(cls):

        try:
            World.set_logic_engine(
                EclipseCLPEngine("ecl-test/info-transfer-test/domaindesc_info_transfer_test.ecl"))
        except SALMAException as e:
            print(e)
            raise
        logger = logging.getLogger('salmalab')
        logger.setLevel(logging.DEBUG)
        ch = logging.StreamHandler()
        logger.addHandler(ch)

    def setUp(self):
        self.logger = logging.getLogger('salmalab')
        World.create_new_world()
        world = World.instance()
        world.load_declarations()

    def test_declarations(self):
        world = World.instance()
        world.initialize(False)
        channels = list(world.get_channels())
        channel_names = sorted([c.name for c in channels])
        print(channel_names)
        self.assertEqual(len(channels), 2)
        self.assertListEqual(channel_names, ["con2rob", "rob2rob"])
        for c in channels:
            qc = world.getFluentValue("channel_in_queue", [c.name])
            self.assertListEqual(qc, [])

        #: :type: Channel
        c1 = world.get_connector("con2rob")
        self.assertTrue(isinstance(c1, Channel))
        self.assertEqual(c1.mode, Channel.MULTICAST)
        self.assertTupleEqual(c1.role1, ("con", "controller"))
        self.assertTupleEqual(c1.role2, ("r", "robot"))

        #: :type: Channel
        c2 = world.get_connector("rob2rob")
        self.assertTrue(isinstance(c2, Channel))
        self.assertEqual(c2.mode, Channel.UNICAST)
        self.assertTupleEqual(c2.role1, ("r1", "robot"))
        self.assertTupleEqual(c2.role2, ("r2", "robot"))


        sensors = list(world.get_sensors())
        sensor_names = sorted([s.name for s in sensors])
        self.assertEqual(len(sensors), 1)
        self.assertListEqual(sensor_names, ["batteryLevelL"])

        sensor_entities = world.getDomain("sensor")
        for s in sensor_entities:
            self.assertTrue(s.id in sensor_names)
            self.assertEqual(s.sortName, "sensor")

        #: :type: Sensor
        s1 = world.get_connector("batteryLevelL")
        self.assertTrue(isinstance(s1, Sensor))
        self.assertEqual(s1.name, "batteryLevelL")
        self.assertEqual(s1.owner_type, "robot")
        self.assertEqual(s1.source_fluent, "batteryLevel")

        remote_sensors = list(world.get_remote_sensors())
        remote_sensor_names = sorted([s.name for s in remote_sensors])
        self.assertEqual(len(remote_sensors), 1)
        self.assertListEqual(remote_sensor_names, ["batteryLevelR"])

        for rs in remote_sensors:
            qc = world.getFluentValue("channel_in_queue", [rs.name])
            self.assertListEqual(qc, [])

        remote_sensor_entities = world.getDomain("remoteSensor")
        for s in remote_sensor_entities:
            self.assertTrue(s.id in remote_sensor_names)
            self.assertEqual(s.sortName, "remoteSensor")

        #: :type: RemoteSensor
        rs1 = world.get_connector("batteryLevelR")
        self.assertTrue(isinstance(rs1, RemoteSensor))
        self.assertEqual(rs1.name, "batteryLevelR")
        self.assertEqual(rs1.remote_sensor_owner_type, "controller")
        self.assertEqual(rs1.local_sensor_name, "batteryLevelL")
        self.assertEqual(rs1.local_sensor_owner_type, "robot")

        channel_entities = world.getDomain("channel")
        for c in channel_entities:
            self.assertTrue(c.id in (channel_names + remote_sensor_names))
            self.assertTrue(c.sortName == "channel" or c.sortName == "remoteSensor")


if __name__ == '__main__':
    unittest.main()
