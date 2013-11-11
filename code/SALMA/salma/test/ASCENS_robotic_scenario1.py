'''
Created on 13.09.2013

@author: christian
'''
import datetime
import logging
import math
import time
import unittest
import io

from salma import SMCException
from salma.engine import EclipseCLPEngine, Engine
from salma.model.core import Fluent, DeterministicAction, \
    ExogenousAction, RandomActionOutcome, UniformStochasticAction, \
    ParametricStochasticAction, Agent, Entity, NOP_OUTCOME
from salma.model.distributions import BernoulliDistribution, \
    UniformDistribution, ArgumentIdentityDistribution
from salma.model.evaluationcontext import EvaluationContext
from salma.model.procedure import Sequence, Procedure, While, \
    Iterate, ActionExecution, Variable, VariableAssignment
from salma.model.world import World
import numpy as np


class Robot(Agent):
    def __init__(self, entityId, initX, initY, initVelocity, initDirection, radius=10.0):

        self.__initX = initX
        self.__initY = initY
        self.__initVelocity = initVelocity
        self.__initDirection = initDirection
        self.__radius = radius

        body = While(EvaluationContext.PYTHON_EXPRESSION,
                     "True", [],
                     Sequence([
                         Iterate(EvaluationContext.ECLP_FUNCTION, 'isSortOf',
                                 [('sensor', 'distance_sensor'), 'distance_sensor'],
                                 ActionExecution('update_distance_sensor',
                                                 [Entity.SELF, Variable('sensor')]
                                 )
                         ),
                         Iterate(EvaluationContext.ECLP_FUNCTION, 'isSortOf',
                                 [('sensor', 'light_sensor'), 'light_sensor'],
                                 ActionExecution('update_light_sensor',
                                                 [Entity.SELF, Variable('sensor')]
                                 )
                         ),
                         VariableAssignment('newDirection',
                                            EvaluationContext.TRANSIENT_FLUENT,
                                            'direction_arbiter',
                                            [Entity.SELF]),

                         ActionExecution('update_direction',
                                         [Entity.SELF, Variable('newDirection')]
                         )
                     ])
        )
        controlProcedure = Procedure('main', [], body)
        Agent.__init__(self, entityId, 'robot', controlProcedure)

    def initialize(self, overwriteFluents=False):
        world = World.getInstance()
        world.setConstantValue('object_radius', [self.id], self.__radius)

        if overwriteFluents:
            world.setFluentValue('xpos', [self.id], self.__initX)
            world.setFluentValue('ypos', [self.id], self.__initY)
            world.setFluentValue('velocity', [self.id], self.__initVelocity)
            world.setFluentValue('direction', [self.id], self.__initDirection)

        lightSensors = world.getDomain('light_sensor')
        distanceSensors = world.getDomain('distance_sensor')
        for l in lightSensors:
            world.setFluentValue('light_sensor_value', [self.id, l.id], 0.0)
        for d in distanceSensors:
            world.setFluentValue('distance_sensor_value', [self.id, d.id], 0.0)

    def getMainFluents(self):
        world = World.getInstance()
        mf = dict()
        mf['xpos'] = world.getFluentValue('xpos', [self.id])
        mf['ypos'] = world.getFluentValue('ypos', [self.id])
        return mf


class RoboticScenario(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        try:
            World.logicsEngine = EclipseCLPEngine("../test/ASCENS_robotic_scenario/ASCENS_robotic_domain.ecl",
                                                  "../test/ASCENS_robotic_scenario/ASCENS_robotic_scenario_01.ecl")
        except SMCException as e:
            print(e)
            raise
        RoboticScenario.logger = logging.getLogger('agamemnon-smc')
        RoboticScenario.logger.setLevel(logging.INFO)
        ch = logging.StreamHandler()
        RoboticScenario.logger.addHandler(ch)

    def setUp(self):
        self.dataFile = None
        self.dataBuffer = None

        World.createNewWorld()
        world = World.getInstance()
        world.addFluent(Fluent("xpos", "float", [("r", "robot")], range=(20, 480)))
        world.addFluent(Fluent("ypos", "float", [("r", "robot")], range=(20, 480)))

        world.addFluent(Fluent("attached", "boolean", [("r", "robot"), ("i", "item")]))

        world.addFluent(Fluent("direction", "float", [("r", "robot")], range=(0, 2 * math.pi)))
        world.addFluent(Fluent("velocity", "float", [("r", "robot")], range=(0, 5.0)))

        world.addFluent(Fluent("light_sensor_value", "float", [("r", "robot"), ("s", "light_sensor")], range=(0, 1.0)))
        world.addFluent(
            Fluent("distance_sensor_value", "float", [("r", "robot"), ("s", "distance_sensor")], range=(0, 1.0)))

        world.addFluent(Fluent("item_sensor_value", "item", [("r", "robot")]))
        world.addFluent(Fluent("target_wavelength", "wavelength", [("r", "robot")]))
        world.addFluent(Fluent("light_active", "boolean", [("l", "light_source")]))

        update_direction = ParametricStochasticAction('update_direction',
                                                      [
                                                          ('rob', 'robot'),
                                                          ('direction', 'float'),
                                                          ('error', UniformDistribution('float', (0, 0)))
                                                      ], immediate=False)

        world.addAction(update_direction)

        update_velocity = ParametricStochasticAction('update_velocity',
                                                     [
                                                         ('rob', 'robot'),
                                                         ('velocity', 'float'),
                                                         ('error', UniformDistribution('float', (0, 0)))
                                                     ], immediate=False)

        world.addAction(update_velocity)

        update_light_sensor = ParametricStochasticAction('update_light_sensor',
                                                         [
                                                             ('rob', 'robot'),
                                                             ('sensor', 'light_sensor'),
                                                             ('wavelength', 'wavelength'),
                                                             ('error', UniformDistribution('float', (0, 0)))
                                                         ], immediate=True)
        world.addAction(update_light_sensor)

        update_distance_sensor = ParametricStochasticAction('update_distance_sensor',
                                                            [
                                                                ('rob', 'robot'),
                                                                ('sensor', 'distance_sensor'),
                                                                ('error', UniformDistribution('float', (0, 0)))
                                                            ], immediate=True)
        world.addAction(update_distance_sensor)

        world.addAction(
            DeterministicAction('grab',
                                [
                                    ('rob', 'robot'),
                                    ('i', 'item')
                                ], immediate=False)
        )

        world.addAction(
            DeterministicAction('drop',
                                [
                                    ('rob', 'robot'),
                                    ('i', 'item')
                                ], immediate=False)
        )

        world.addAction(
            UniformStochasticAction('sense_for_item', [('r', 'robot')],
                                    [RandomActionOutcome('update_item_sensor', [('r', 'robot')]),
                                     NOP_OUTCOME
                                    ]
            )
        )


    def tearDown(self):
        if self.dataFile is not None:
            if not self.dataFile.closed:
                self.dataFile.flush()
            self.dataFile.close()


    def reportPositions(self, world, step, deltaT):
        row = [str(step)]
        robots = world.getDomain('robot')
        for r in robots:
            x = world.getFluentValue('xpos', [r.id])
            y = world.getFluentValue('ypos', [r.id])
            direction = world.getFluentValue('direction', [r.id])
            row.append("{:.5f}".format(x))
            row.append("{:.5f}".format(y))
            row.append("{:.5f}".format(direction))
        line = ";".join(row) + "\n"
        self.dataBuffer.write(line.encode(encoding="ascii"))
        print(str(datetime.timedelta(seconds=deltaT)), row)

    def initBasicScenario(self, numOfRobots, randomize=True, lightSources=[]):

        world = World.getInstance()
        for i in range(numOfRobots):
            r = Robot('rob' + str(i + 2), 0.0, 0.0, 0.0, 0.0)
            world.addAgent(r)

        world.addEntity(Entity('worldbounds', 'boundary'))
        for i in range(1, 25):
            ls_id = 'ls' + str(i)
            world.addEntity(Entity(ls_id, 'light_sensor'))
            ds_id = 'ds' + str(i)
            world.addEntity(Entity(ds_id, 'distance_sensor'))

        # ls is (x,y) tuple
        for i in range(len(lightSources)):
            world.addEntity(Entity('light_source_' + str(i + 1), 'light_source'))

        world.initialize(randomize)

        for i in range(1, 25):
            ls_id = 'ls' + str(i)
            ds_id = 'ds' + str(i)
            world.setConstantValue('sensor_angle', [ls_id],
                                   (i - 1) * 2 * math.pi / 24)
            world.setConstantValue('sensor_angle', [ds_id],
                                   (i - 1) * 2 * math.pi / 24)

        world.setConstantValue('rectangle_width', ['worldbounds'], 500.0)
        world.setConstantValue('rectangle_height', ['worldbounds'], 500.0)

        world.setFluentValue('xpos', ['worldbounds'], 0.0)
        world.setFluentValue('ypos', ['worldbounds'], 0.0)

        world.setConstantValue('light_sensor_count', [], 24)
        world.setConstantValue('distance_sensor_count', [], 24)
        world.setConstantValue('max_light_sensor_range', [], 300.0)
        world.setConstantValue('max_light_sensor_angle', [], math.pi * 0.5)
        world.setConstantValue('max_distance_sensor_range', [], 20.0)
        world.setConstantValue('max_distance_sensor_angle', [], math.pi * 0.5)
        world.setConstantValue('max_light_sensor_intensity', [], 5.0)

        for i, ls in enumerate(lightSources):
            lsid = 'light_source_' + str(i + 1)
            world.setFluentValue('xpos', [lsid], ls[0])
            world.setFluentValue('ypos', [lsid], ls[1])

        robots = world.getDomain('robot')
        lights = world.getDomain('light_source')

        for r in robots:
            if isinstance(r, Robot):
                r.initialize(not randomize)
                print("Initialized Robot {} : {}".format(r.id, r.getMainFluents()))

        for l in lights:
            world.setConstantValue('object_radius', [l.id], 10.0)


    #@unittest.skip
    def testScenario(self):
        world = World.getInstance()
        #         rob1 = Robot("rob1", 200, 200, 2, math.radians(45))
        #         rob2 = Robot("rob2", 50, 50, 2, math.radians(270))
        #         rob3 = Robot("rob3", 400, 50, 2, math.radians(0))
        #
        #         world.addAgent(rob1)
        #         world.addAgent(rob2)
        #         world.addAgent(rob3)
        self.initBasicScenario(5, randomize=True, lightSources=[(450, 450), (50, 400), (50, 50)])

        self.__datalog = []

        world.printState()
        fileName = "robotic_scenario_positions_{}.csv".format(
            datetime.datetime.now().strftime("%Y-%m-%d_%H-%M-%S"))
        self.dataFile = io.FileIO(fileName, mode="w")
        self.dataBuffer = io.BufferedWriter(self.dataFile, buffer_size=4 * io.DEFAULT_BUFFER_SIZE)

        res = world.runUntilFinished(maxRealTime=datetime.timedelta(minutes=2),
                                     stepListeners=[self.reportPositions])

        self.dataFile.flush()
        self.dataFile.close()
        print(res)


    @unittest.skip
    def test_DirectionArbiter(self):
        world = World.getInstance()

        seq = Sequence([
            Iterate(EvaluationContext.ECLP_FUNCTION, 'isSortOf',
                    [('sensor', 'distance_sensor'), 'distance_sensor'],
                    ActionExecution('update_distance_sensor',
                                    [Entity.SELF, Variable('sensor')]
                    )
            ),
            Iterate(EvaluationContext.ECLP_FUNCTION, 'isSortOf',
                    [('sensor', 'light_sensor'), 'light_sensor'],
                    ActionExecution('update_light_sensor',
                                    [Entity.SELF, Variable('sensor')]
                    )
            ),
            VariableAssignment('newDirection',
                               EvaluationContext.TRANSIENT_FLUENT,
                               'direction_arbiter',
                               [Entity.SELF]),

            ActionExecution('update_direction',
                            [Entity.SELF, Variable('newDirection')]
            )
        ])

        rob1 = Agent("rob1", "robot", Procedure("main", [], seq))
        world.addAgent(rob1)
        self.initBasicScenario(0, False)

        world.setFluentValue('xpos', [rob1.id], 20)
        world.setFluentValue('ypos', [rob1.id], 470)
        world.setFluentValue('direction', [rob1.id], math.radians(45))
        world.setFluentValue('velocity', [rob1.id], 1)
        world.setConstantValue('object_radius', [rob1.id], 10.0)

        lightSensors = world.getDomain('light_sensor')
        distanceSensors = world.getDomain('distance_sensor')
        for l in lightSensors:
            world.setFluentValue('light_sensor_value', [rob1.id, l.id], 0.0)
        for d in distanceSensors:
            world.setFluentValue('distance_sensor_value', [rob1.id, d.id], 0.0)

        print("-" * 50)
        print("Before")
        world.printState()

        world.runUntilFinished()
        print("-" * 50)
        print("\n\nAfter\n\n")
        world.printState()

        print("New Direction: {}".format(
            math.degrees(world.getFluentValue('direction', [rob1.id]))))

        self.showSensors(rob1.id)

    def showSensors(self, robotId):
        world = World.getInstance()
        distance_sensors = world.getDomain('distance_sensor')
        sensorValues = []
        for ds in distance_sensors:
            phi = world.getConstantValue('sensor_angle', [ds.id])
            v = world.getFluentValue('distance_sensor_value', [robotId, ds.id])
            sensorValues.append((phi, ds.id, v))
        sensorValues.sort()
        for sv in sensorValues:
            print("{:>5}: {:>7.2f} --> {:03.2f}".format(sv[1], math.degrees(sv[0]), sv[2]))


if __name__ == "__main__":
    #import sys;sys.argv = ['', 'Test.testName']
    unittest.main()
    