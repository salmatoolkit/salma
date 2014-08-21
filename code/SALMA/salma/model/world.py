"""
Created on 21.05.2013

@author: christian
"""
import itertools
import logging
import random
import datetime
import time

import pyclp

from salma.SALMAException import SALMAException
from salma.constants import *
from salma.model.actions import StochasticAction, DeterministicAction, ExogenousAction, RandomActionOutcome, \
    Deterministic, Uniform
from salma.model.core import Constant, Action
from salma.model.evaluationcontext import EvaluationContext
from ..engine import Engine
from salma.statistics import HypothesisTest
from .core import Entity, Fluent
from .agent import Agent
from .procedure import Variable, Act
from salma.model.infotransfer import Connector, Channel, Sensor, RemoteSensor
from collections.abc import Iterable

MODULE_LOGGER_NAME = 'agamemnon-smc.world'
moduleLogger = logging.getLogger(MODULE_LOGGER_NAME)


class World(Entity):
    """
    Singleton class that acts as the *center of the simulation*. The domain model is loaded with load_declaration and
    all actions can be configured using the appropriate configuration classes. The configuration can then be checked with
    several check_* methods. After that, the domain models are set up with initialize, optionally also generating a randomized
    initial situation by sampling values for all fluent and constant instances. Additionally, the fluent and constant values
    can also be manipulated with
    """

    INVARIANT, ACHIEVE, ACHIEVE_AND_SUSTAIN = range(3)

    __logic_engine = None

    @staticmethod
    def logic_engine():
        """
        The world's logic engine.
        :rtype: Engine
        """
        return World.__logic_engine

    @staticmethod
    def set_logic_engine(engine):
        """
        Sets the static logic engine reference.
        :type engine: Engine
        """
        World.__logic_engine = engine

    SORT_NAME = 'world'
    ENTITY_ID = 'world'

    __instance = None

    def __init__(self):
        Entity.__init__(self, World.ENTITY_ID, World.SORT_NAME)

        # fluentName -> core.Fluent
        self.__fluents = dict()

        #: :type: dict[str, (str, str, list)]
        self.__derived_fluents = dict()

        #: :type: dict[str, Constant]
        self.__constants = dict()
        # action_name -> core.Action
        self.__actions = dict()

        self.__exogenousActions = dict()


        self.__virtualSorts = set(["sort", "message"])

        # store all entities in a sort -> entity dict 
        self.__domainMap = dict()
        # agents is a dict that stores
        self.__entities = dict()
        self.__agents = dict()

        # ------------------- information transfer ---------------------
        #: :type: dict[str, Connector]
        self.__connectors = dict()

        # ------------------- information transfer end ---------------------

        self.__expressionContext = dict()

        self.__finished = False
        self.__initialized = False

        #: :type: dict[str, object]
        self.__additional_expression_context_globals = dict()

        # dict with registered properties: name -> (formula, property_type)
        #: :type: dict[str, (str, int)]
        self.__invariants = dict()
        #: :type: dict[str, (str, int)]
        self.__achieve_goals = dict()
        #: :type: dict[str, (str, int)]
        self.__achieve_and_sustain_goals = dict();

        #: :type: set[str]
        self.__already_achieved_goals = set()

        if World.logic_engine() is None:
            raise SALMAException("Engine not set when creating world.")
        World.logic_engine().reset()
        self.addFluent(Fluent("time", "integer", []))
        self.__evaluationContext = LocalEvaluationContext(self, None)

    @property
    def virtual_sorts(self):
        """
        :return: set[str]
        """
        return self.__virtualSorts

    @virtual_sorts.setter
    def virtual_sorts(self, vsorts):
        """
        :type vsorts: list[str]
        """
        self.__virtualSorts = set(vsorts)

    def getExpressionContext(self):
        return self.__expressionContext

    def get_additional_expression_context_globals(self):
        return self.__additional_expression_context_globals

    def add_additional_expression_context_global(self, name, value):
        """
        Adds a global fixed symbol to the expression context. This can be used to declare modules or constants.

        :param str name: the name of the new symbol
        :param object value: the value
        """
        self.__additional_expression_context_globals[name] = value

    @staticmethod
    def create_new_world():
        """
        Creates a new instance for the singleton World and returns it.
        :rtype: World
        """
        World.__instance = World()
        return World.__instance

    def evaluation_context(self):
        return self.__evaluationContext

    def enumerate_fluent_instances(self, fluent):
        """
        Creates an iterator for instances of the given fluent
        :type fluent: Fluent
        :rtype: list of list of str
        """
        candidates = []
        candidate_indexes = []
        for p in fluent.parameters:
            dom = self.getDomain(p[1])
            # if the parameter's domain is empty, leave
            if len(dom) == 0:
                return
            candidates.append(list(dom))
            candidate_indexes.append(0)

            # todo: handle fluents without parameters
        finished = False
        while not finished:
            param_selection = []
            for i, c in enumerate(candidates):
                param_selection.append(c[candidate_indexes[i]].id)
            yield param_selection

            # increment first position similar to binary counter
            if len(candidate_indexes) > 0:
                candidate_indexes[0] += 1
                # propagate increment
                for i, c in enumerate(candidates):
                    if candidate_indexes[i] > len(c) - 1:
                        # stop at the last candidate list = parameter position
                        if i == len(candidates) - 1:
                            finished = True
                        else:
                            candidate_indexes[i] = 0
                            candidate_indexes[i + 1] += 1
            else:
                finished = True

    def __initialize_fluent(self, fluent):
        """
        Creates samples for the fluent instance for each combination
            of parameter values.

        :type fluent: Fluent
        """
        # list of domain lists
        for paramSelection in self.enumerate_fluent_instances(fluent):
            value = fluent.generateSample(self.__evaluationContext, paramSelection)
            if value is not None:
                World.logic_engine().setFluentValue(fluent.name, paramSelection, value)

    def check_fluent_initialization(self):
        """
        Checks whether all instances of all declared fluents and constants are initialized correctly.
        :returns: tuple with (uninitialized_fluent_instances, uninitialized_constant_instances)
        :rtype: (list of (str, list), list of (str, list))
        """
        uninitialized_fluent_instances = []
        uninitialized_constant_instances = []

        for con in self.__constants.values():
            for paramSelection in self.enumerate_fluent_instances(con):
                v = self.getConstantValue(con.name, paramSelection)
                if v is None:
                    instance = (con.name, paramSelection)
                    uninitialized_constant_instances.append(instance)

        for fluent in self.__fluents.values():
            """:type fluent: Fluent """
            for paramSelection in self.enumerate_fluent_instances(fluent):
                instance = None
                v = self.getFluentValue(fluent.name, paramSelection)
                if v is None:
                    instance = (fluent.name, paramSelection)
                    uninitialized_fluent_instances.append(instance)

        return uninitialized_fluent_instances, uninitialized_constant_instances

    def check_action_initialization(self):
        """
        Checks whether all stochastic and exogenous actions have been configured correctly.
        :return: tuple with (problematic_stochastic_actions, problematic_exogenous_actions) where each is a list of
        tuples with (action, promblem_list)

        :rtype: (list[(StochasticAction, list)], list[(ExogenousAction, list)])
        """
        problematic_stochastic_actions = []
        problematic_exogenous_actions = []

        for action in filter(lambda a: isinstance(a, StochasticAction), self.__actions.values()):
            assert isinstance(action, StochasticAction)
            problems = action.check(self.__actions)
            if len(problems) > 0:
                problematic_stochastic_actions.append((action, problems))

        for exo_action in self.__exogenousActions.values():
            assert isinstance(exo_action, ExogenousAction)
            if exo_action.config is None:
                problematic_exogenous_actions.append((exo_action, ["uninitialized"]))
            else:
                problems = exo_action.config.check()
                if len(problems) > 0:
                    problematic_exogenous_actions.append((exo_action, problems))

        return problematic_stochastic_actions, problematic_exogenous_actions

    def __make_fluent_access_function(self, fluent_name):
        def __f(*params):
            return self.getFluentValue(fluent_name, params)
        return __f


    def __make_constant_access_function(self, constant_name):
        def __f(*params):
            return self.getConstantValue(constant_name, params)
        return __f

    def __make_derived_fluent_access_function(self, derived_fluent_name):
        def __f(*params):
            return self.__evaluationContext.evaluateFunction(EvaluationContext.TRANSIENT_FLUENT, derived_fluent_name, *params)
        return __f

    def __create_expression_context(self):
        self.__expressionContext = dict()
        self.__expressionContext.update(self.__additional_expression_context_globals)
        #: :type fluent: Fluent
        for fluent in self.__fluents.values():
            self.__expressionContext[fluent.name] = self.__make_fluent_access_function(fluent.name)
        for df in self.__derived_fluents.values():
            self.__expressionContext[df[0]] = self.__make_derived_fluent_access_function(df[0])
        for con in self.__constants.values():
            self.__expressionContext[con.name] = self.__make_constant_access_function(con.name)

        #todo: include derived fluents in expression context?
        def __fluentChangeClock(fluentName, *params):
            return self.getFluentChangeTime(fluentName, params)

        def __actionClock(actionName, *params):
            return self.getActionClock(actionName, params)
        #todo: add action count
        self.__expressionContext['fluentClock'] = __fluentChangeClock
        self.__expressionContext['actionClock'] = __actionClock
        # add a "variable" for each entity to allow access without quotation marks
        for id in self.__entities.keys():
            self.__expressionContext[str(id)] = str(id)


    def sample_fluent_values(self):
        """
        Creates samples for all instances of all fluents except 'time'
        """
        for fluent in itertools.filterfalse(lambda f: f.name == 'time', self.__fluents.values()):
            self.__initialize_fluent(fluent)

    def __load_stochastic_action_declarations(self, declarations, immediate_action_names):
        """
        Loads the stochastic declarations and creates StochasticAction objects accordingly.
        :type declarations: list of (str, list of object, list of str)
        :type immediate_action_names: list of str
        """
        for sa in declarations:
            immediate = sa[0] in immediate_action_names
            params = sa[1]
            outcome_names = sa[2]
            outcomes = []
            for o_name in outcome_names:
                outcome_action = self.getAction(o_name)
                outcomes.append(RandomActionOutcome(outcome_action))
            strategy = Deterministic() if len(outcomes) == 1 else Uniform()
            action = StochasticAction(sa[0], params, outcomes, strategy, immediate)
            self.addAction(action)

    def load_declarations(self):
        """
        Loads the declarations from the domain specification and initializes fluents, constants, and actions.
        """
        # fluentName -> core.Fluent
        self.__fluents = dict()
        self.__derived_fluents = dict()
        self.__constants = dict()
        # action_name -> core.Action
        self.__actions = dict()
        self.__exogenousActions = dict()
        self.__connectors = dict()

        declarations = World.logic_engine().load_declarations()
        for f in declarations['fluents']:
            self.addFluent(Fluent(f[0], f[2], f[1]))
        for f in declarations['derived_fluents']:
            self.add_derived_fluent(f[0], f[2], f[1])
        for c in declarations['constants']:
            self.addConstant(Constant(c[0], c[2], c[1]))
        for pa in declarations['primitive_actions']:
            immediate = pa[0] in declarations['immediate_actions']
            self.addAction(DeterministicAction(pa[0], pa[1], immediate))

        self.__load_stochastic_action_declarations(declarations["stochastic_actions"],
                                                   declarations["immediate_actions"])

        for ea in declarations['exogenous_actions']:
            self.addExogenousAction(ExogenousAction(ea[0], ea[1], ea[2]))

        # info transfer
        for c in declarations["channels"]:
            chan = Channel(c[0], c[1], c[2], c[3])
            self.add_connector(chan)
            self.addEntity(Entity(chan.name, "channel"))

        for s in declarations["sensors"]:
            sensor = Sensor(s[0], s[1], s[2])
            self.add_connector(sensor)
            self.addEntity(Entity(sensor.name, "sensor"))

        for rs in declarations["remote_sensors"]:
            remote_sensor = RemoteSensor(rs[0], rs[1], rs[2], rs[4])
            self.add_connector(remote_sensor)
            self.addEntity(Entity(remote_sensor.name, "remoteSensor"))


    def sync_domains(self):
        self.__domainMap = dict()
        domain_map_from_engine = World.logic_engine().initSortHierarchy()

        for sort, domain in domain_map_from_engine.items():
            self.__domainMap[sort] = set()
            for entityId in domain:
                entity = None
                try:
                    entity = self.__entities[entityId]
                except KeyError:
                    #raise SALMAException("No Entity instance registered for {}:{}".format(entityId, sort))
                    entity = Entity(entityId, sort)
                    self.__entities[entityId] = entity
                self.__domainMap[sort].add(entity)

    def initialize(self, sample_fluent_values=True, removeFormulas=True, deleteConstants=True):
        """
        1. Sets up domains, i.e. defines the sets of entity objects for each sort.

        2. Optionally sets ups the a new initial situation by creating samples for the fluent instance for each combination
            of parameter values.
        """
        World.logic_engine().reset(removeFormulas=removeFormulas, deleteConstants=deleteConstants)
        self.__evaluationContext = LocalEvaluationContext(self, None)

        for sort in self.__domainMap.keys():
            oids = []
            for entity in self.__domainMap[sort]:
                oids.append(entity.id)
            World.logic_engine().defineDomain(sort, oids)

        #keep entityId->entity maps (__entities & __agents)

        self.sync_domains()

        World.logic_engine().setFluentValue('time', [], 0)

        if sample_fluent_values:
            self.sample_fluent_values()

        self.__initialized = True
        #self.__persistentPropertiesInitialized = False
        self.__create_expression_context()

    def is_finished(self):
        """
        Returns true if every registered agent has finished already.

        :rtype: bool
        """
        return self.__finished

    def getAllEntities(self):
        """
        Returns all registered entities, i.e. passive entities and agents.
        :rtype: list
        """
        return self.__entities.values()

    def getAgents(self, sort=None):
        """
        Returns a list of all agents, optionally restricted to the given sort.
        :rtype: list[Agent]
        """
        entities = self.getAllEntities() if sort is None else self.getDomain(sort)
        result = []
        for e in entities:
            if isinstance(e, Agent):
                result.append(e)
        return result

    def getEntityById(self, entityId):
        """
        Returns the entity with the given id or None if no entity with that id is found.
        :type entityId: str
        :rtype: Entity
        """
        if not entityId in self.__entities:
            return None
        else:
            return self.__entities[entityId]

    def addEntity(self, entity):
        """
        Adds the given entity to the registry
        :type entity: Entity
        """
        self.__entities[entity.id] = entity
        if isinstance(entity, Agent):
            self.__agents[entity.id] = entity
        if entity.sortName in self.__domainMap:
            self.__domainMap[entity.sortName].add(entity)
        else:
            self.__domainMap[entity.sortName] = set([entity])

    def addAgent(self, agent):
        """
        Adds the given agent to the entity registry and set this world instance as the agent's evaluation context.
        :type agent: Agent
        """
        self.addEntity(agent)
        agent.evaluation_context = LocalEvaluationContext(agent, None)

    def removeEntity(self, entity):
        """
        Removes the given entity from the registry.
        :param entity: can either be an Entity or a string that specifies the id
        :type entity: Entity
        :raises: an SALMAException if the given entity was not found in the registry.
        """
        if not entity.sortName in self.__domainMap:
            raise SALMAException("Trying to remove entity {} with unregistered sort {}".format(
                entity.id, entity.sortName))
        eset = self.__domainMap[entity.sortName]
        if (not entity in eset) or (not entity.id in self.__entities):
            raise SALMAException("Trying to remove unregistered entity {}.".format(entity.id))
        eset.remove(entity)
        self.__entities.pop(entity.id)
        if isinstance(entity, Agent):
            try:
                self.__agents.pop(entity.id)
            except KeyError:
                raise SALMAException("Trying to remove unregistered agent {} from agent list.".format(entity.id))
                # empty sorts are left in the domain map

    def removeAgent(self, agent):
        """
        Removes the given agent and sets its evaluation context to None.
        :type agent: Agent
        """
        self.removeEntity(agent)
        agent.evaluation_context = None

    def addAction(self, action):
        """
        Adds the given action to the action registry. This method is mostly not called directly but automatically
        by load_declaration.

        :type action: Action
        """
        self.__actions[action.name] = action

    def removeAction(self, action):
        """
        Removes the given action.
        :raises SALMAException if action is unregistered.
        :type action: Action
        """
        try:
            del self.__actions[action.name]
        except KeyError:
            raise SALMAException("Trying to remove unregistered action %s ." % action.name)


    def getAction(self, action_name):
        """
        Returns the action (either deterministic or stochastic) with the given name.
        :raises SALMAException if action is unregistered.
        :type action_name: str
        :rtype: Action
        """
        try:
            return self.__actions[action_name]
        except KeyError:
            raise (SALMAException("Action {} not registered.".format(action_name)))

    def get_stochastic_action(self, action_name):
        """
        Returns the stochastic action with the given name.
        :type action_name: str
        :rtype: StochasticAction
        """
        a = self.getAction(action_name)
        if not isinstance(a, StochasticAction):
            raise SALMAException("Action %s is not stochastic." % action_name)
        return a

    def getAllActions(self):
        """
        Returns a list with all registered deterministic and stochastic actions.
        :rtype: list
        """
        return self.__actions.values()

    def addExogenousAction(self, exogenousAction):
        """
        Registers the given exogenous action. Normally this method is called automatically by load_declarations().
        :type exogenousAction: ExogenousAction
        """
        self.__exogenousActions[exogenousAction.action_name] = exogenousAction

    def removeExogenousAction(self, exogenousAction):
        """
        Removes the given exogenous action.
        :type exogenousAction: ExogenousAction
        """
        try:
            del self.__exogenousActions[exogenousAction.action_name]
        except KeyError:
            raise (
                SALMAException("Trying to erase unregistered exogenous action {}.".format(exogenousAction.action_name)))

    def get_exogenous_action(self, action_name):
        """
        Returns the exogenous action with the given name.
        :raises: SALMAException if there is no exogenous action with the given name.
        :type action_name: str
        :rtype: ExogenousAction
        """
        try:
            return self.__exogenousActions[action_name]
        except KeyError:
            raise (SALMAException("Unregistered exogenous action {}.".format(action_name)))

    def get_exogenous_actions(self):
        """
        Returns a list view on all exogenous actions.
        :rtype: list of ExogenousAction
        """
        return self.__exogenousActions.values()

    def getSorts(self):
        """
        Returns a key view of all sort names.
        :rtype: builtins.key_values
        """
        return self.__domainMap.keys()

    def getDomain(self, sort_name):
        """
        Returns the list of all entities that make the domain of the given sort.
        :type sort_name: str
        :rtype: list of Entity
        """
        try:
            return self.__domainMap[sort_name]
        except KeyError:
            raise (SALMAException("Sort {} not declared.".format(sort_name)))

    def addFluent(self, fluent):
        """
        Adds a core.Fluent object to the metamodel.
        NOTE: this method should normally not be called directly but is automatically called in World.load_declarations().
        :type fluent: Fluent
        """
        self.__fluents[fluent.name] = fluent

    def add_derived_fluent(self, fluent_name, fluent_type, params):
        self.__derived_fluents[fluent_name] = (fluent_name, fluent_type, params)

    def addConstant(self, con):
        """
        Adds a constant to the metamodel.
        NOTE: this method should normally not be called directly but is automatically called in World.load_declarations().
        :type con: Constant
        """
        self.__constants[con.name] = con

    def add_connector(self, connector: Connector):
        """
        Adds a connector, i.e. a channel, soensor, or remote sensor, to the world instance. This method is
        used mainly internally in load_declarations().
        :param connector: the connector to add
        """
        self.__connectors[connector.name] = connector



    def getFluents(self):
        """
        Returns a list view of all fluents currently registered in the metamodel as a list of core.Fluent objects.
        This list also included constants.
        :rtype: builtins.dict_values
        """
        return self.__fluents.values()

    def get_derived_fluents(self):
        """

        :return:
        """
        return self.__derived_fluents.values()

    def getConstants(self):
        """
        Returns a list of all registered constants.
        :rtype: list
        """
        return self.__constants.values()

    def getFluent(self, fluentName):
        """
        Returns the core.Fluent object associated by the given fluent name or None if such a fluent
        hasn't been registered.
        :type fluentName: str
        :rtype: Fluent
        """
        if fluentName in self.__fluents:
            return self.__fluents[fluentName]
        else:
            return None

    def getConstant(self, constantName):
        """
        Returns the core.Constant object associated by the given constant name or None if such a constant
        hasn't been registered.
        :type constantName: str
        :rtype: Constant
        """
        if constantName in self.__constants:
            return self.__constants[constantName]
        else:
            return None

    def get_connectors(self):
        """
        Returns the list of registered connectors (channels, sensors and remote sensors).
        :return: the list of connectors
        :rtype: Iterable[Connector]
        """
        return self.__connectors.values()

    def get_connector(self, name: str) -> Connector:
        """
        Returns the connector (channel, sensor, remote sensor) with the given name or None if the name
        is unregistered.
        :param name: the name of the connector.
        :return: the connector
        """
        if name in self.__connectors:
            return self.__connectors[name]
        else:
            return None

    def get_channels(self):
        """
        Returns the registered channels.
        :rtype: Iterable[Channel]
        """
        return (chan for chan in self.__connectors.values() if isinstance(chan, Channel))

    def get_sensors(self):
        """
        Returns the registered (local/direct) sensors.
        :rtype: Iterable[Sensor]
        """
        return (s for s in self.__connectors.values() if isinstance(s, Sensor))

    def get_remote_sensors(self):
        """
        Returns the registered remote sensors.
        :rtype: Iterable[RemoteSensor]
        """
        return (s for s in self.__connectors.values() if isinstance(s, RemoteSensor))

    @staticmethod
    def instance():
        """
        Returns the singleton instance of the World.
        :rtype: World
        """
        if World.__instance is None:
            World.__instance = World()
        return World.__instance

    def getFluentValue(self, fluent_name, fluent_params):
        """
        Returns the current value of the fluent instance that is given by the fluent name and the parameter list.
        :type fluent_name: str
        :type fluent_params: list
        :rtype: object
        """
        # we don't check if the fluent has been registered for performance reasons
        fv = World.logic_engine().getFluentValue(fluent_name, *fluent_params)
        if fv is None:
            return None
        else:
            return fv.value

    def getTime(self):
        """
        Returns the current value of the fluent 'time'.
        :rtype: int
        """
        return self.getFluentValue('time', [])

    def getConstantValue(self, constantName, constantParams):
        """
        Returns the current value of the given constant instance.
        :type constantName: str
        :type constantParams: list
        :rtype: object
        """
        return World.logic_engine().getConstantValue(constantName, constantParams)

    def setFluentValue(self, fluentName, fluentParams, value):
        """
        Sets the current value for the given fluent instance.
        :type fluentName: str
        :type fluentParams: list
        :type value: object
        """
        World.logic_engine().setFluentValue(fluentName, fluentParams, value)

    def setConstantValue(self, constantName, constantParams, value):
        """
        Sets the current value for the given constant instance.
        :type constantName: str
        :type constantParams: list
        :type value: object
        """
        World.logic_engine().setConstantValue(constantName, constantParams, value)

    def __translate_action_execution(self, evaluation_context, action_execution):
        """
        Generates a ground deterministic action outcome from the given Act as a
        (action_name, [ground_params]) tuple. If actionExecution refers to a stochastic action,
        an outcome is generated according to the distribution that was defined for the action.
        The given evaluation context is used for generating an outcome.

        :return: (str, list)
        :raises: SALMAException if action is not registered.

        :type evaluation_context: EvaluationContext
        :type action_execution: Act
        :rtype: (str, list)
        """
        try:
            action = self.__actions[action_execution.actionName]
        except KeyError:
            raise (
                SALMAException("Trying to execute unregistered action: {}".format(
                    action_execution.actionName)))

        ground_params = evaluation_context.resolve(*action_execution.actionParameters)

        if isinstance(action, StochasticAction):
            return action.generateOutcome(evaluation_context, ground_params)
        else:
            return action_execution.actionName, ground_params

    @staticmethod
    def __get_action_name_from_term(action_term):
        """
        Extracts the action name from the given term. This is either the functor if action_term is a Compound or
        the string representation of action_term.
        :param action_term: (pyclp.Atom, pyclp.Compound)
        :rtype: str
        """
        if isinstance(action_term, pyclp.Compound):
            return action_term.functor()
        else:
            return str(action_term)

    def get_exogenous_action_instances(self):
        """
        Creates a list of all exogenous action instances (see ExogenusAction.generate_instance) for
        all possible candidates. The list of possible candidates is calculated by calling
        Engine.getExogenousActionCandidates() first. Then ExogenousAction.shouldHappen() is used to
        select instances that should occur.
        :rtype: list
        """
        candidates = World.logic_engine().getExogenousActionCandidates()
        ea_instances = []
        for candidate in candidates.items():
            action_name = candidate[0]
            combinations = candidate[1]
            # just ignore if the ex action is not registered
            if action_name in self.__exogenousActions:
                ea = self.__exogenousActions[action_name]
                for params in combinations:
                    if ea.should_happen(self.__evaluationContext, params):
                        instance = ea.generate_instance(self.__evaluationContext, params)
                        ea_instances.append(instance)
        return ea_instances

    def step(self, evaluate_properties=True):
        """
        Performs one discrete time step for all agents and runs the progression.

        :returns: (verdict, self.__finished, toplevel_results, scheduled_results, all_actions, [],
                failed_invariants, failed_sustain_goals, failure_stack)
        :rtype: (int, bool, dict[str, int], dict, list, list, set[str], set[str], list)
        """
        actions = []  # list of tuples: (action_execution, params)
        all_actions = []
        # gather actions

        # A set that keeps track of all processes that have been entered already in this step.
        # This is used to by the process to determine whether a new step is started and hence a pending
        # action should be discarded.
        #: :type : set of process.Process
        entered_processes = set()

        while True:
            #: :type: list[process.Process]
            active_processes = []

            # The schedule trigger predicate is evaluated iteratively here. This allows processes to
            # become runnable as an effect by a previous immediate action by another process.
            self.__finished = True
            for agent in self.__agents.values():
                if not agent.is_finished():
                    self.__finished = False
                active_processes.extend(agent.update_schedule())
            immediate_actions = []

            for proc in active_processes:
                action_execution = proc.step(proc not in entered_processes)
                entered_processes.add(proc)
                if action_execution is not None:
                    act = self.__translate_action_execution(proc.current_evaluation_context, action_execution)

                    if act[0] in self.__actions and self.__actions[act[0]].immediate:
                        immediate_actions.append(act)
                    else:
                        actions.append(act)
                        proc.set_pending_action(act)
                        # progress immediate actions without succeeding time step
                        # leave loop only if we don't have any immediate actions left
            if len(immediate_actions) == 0:
                break
            random.shuffle(immediate_actions)
            failed_actions = World.logic_engine().progress(immediate_actions)
            all_actions.extend(immediate_actions)

            if moduleLogger.isEnabledFor(logging.DEBUG):
                moduleLogger.debug("Progressed immediately: %s", immediate_actions)
            if len(failed_actions) > 0:
                return NOT_OK, self.__finished, {}, {}, [], all_actions, failed_actions, set(), set(), []

        ex_act_instances = self.get_exogenous_action_instances()

        if ex_act_instances is not None:
            actions.extend(ex_act_instances)

        # regular action means non-exogenous actions
        failed_regular_actions = []

        if len(actions) > 0:
            # shuffle actions as means for achieving fairness
            random.shuffle(actions)
            failed_actions = World.logic_engine().progress(actions)
            all_actions.extend(actions)
            if moduleLogger.isEnabledFor(logging.DEBUG):
                moduleLogger.debug("Progressed: %s", actions)

            # failed_regular_actions = list(
            #     itertools.filterfalse(lambda fa: World.__get_action_name_from_term(fa) in self.__exogenousActions,
            #                           failed_actions))

            for fa in failed_actions:
                if fa[0] not in self.__exogenousActions:
                    failed_regular_actions.append(fa)

        if len(failed_regular_actions) > 0:
            return (NOT_OK, self.__finished, {}, {}, [], all_actions, failed_regular_actions,
                    set(), set(), [])

        verdict = NONDET
        if evaluate_properties:
            toplevel_results, scheduled_results, scheduled_keys, failure_stack = World.logic_engine().evaluationStep()
            verdict, failed_invariants, failed_sustain_goals = self.__arbitrate_verdict(toplevel_results,
                                                                                        scheduled_results,
                                                                                        scheduled_keys)
        else:
            toplevel_results, scheduled_results, scheduled_keys = dict(), dict(), []
            failure_stack = []
            failed_invariants, failed_sustain_goals = set(), set()

        World.logic_engine().progress([('tick', [])])
        # it's ok if events fail but if regular actions fail, we're in trouble!

        # TODO: distinguish between actions that take time and actions that don't
        # execute all non-time actions before taking a time step
        return (verdict, self.__finished, toplevel_results, scheduled_results, scheduled_keys, all_actions, [],
                failed_invariants, failed_sustain_goals, failure_stack)

    @staticmethod
    def __check_property_success(pname, toplevel_results, scheduled_results):
        if toplevel_results[pname] == OK:
            return True
        if pname in scheduled_results:
            for t, v in scheduled_results[pname]:
                if v == OK:
                    return True

    @staticmethod
    def __check_property_failure(pname, toplevel_results, scheduled_results):
        if toplevel_results[pname] == NOT_OK:
            return True
        if pname in scheduled_results:
            for t, v in scheduled_results[pname]:
                if v == NOT_OK:
                    return True

    def __arbitrate_verdict(self, toplevel_results, scheduled_results, scheduled_keys):
        """
        Calculates an overall decision whether the current experiment should be counted as
        a success or not. A run is a success if all achieve goals are true. A
        :param dict[str, int] toplevel_results: the toplevel results
        :param dict[str, list[(int,int)]] scheduled_results: scheduled results
        :param dict[str, list[int]] scheduled_keys: scheduled keys
        :returns: Returns a tuple: verdict, list of decision reasons
        :rtype: (int, set, set)
        """
        failed_invariants = set()
        pending_properties = set()
        failed_sustain_goals = set()
        verdict = NONDET

        for pname in self.__invariants.keys():
            if pname not in toplevel_results:
                raise SALMAException("Invariant {} was not contained in the evaluation "
                                     "step's top-level results ({})!".format(pname, toplevel_results))

            if World.__check_property_failure(pname, toplevel_results, scheduled_results):
                verdict = NOT_OK
                failed_invariants.add(pname)

        for pname in self.__achieve_goals.keys():
            if pname not in toplevel_results:
                raise SALMAException("Achieve goal {} was not contained in the evaluation "
                                     "step's top-level results ({})!".format(pname, toplevel_results))

            if World.__check_property_success(pname, toplevel_results, scheduled_results):
                self.__already_achieved_goals.add(pname)

        for pname in self.__achieve_and_sustain_goals.keys():
            if pname not in toplevel_results:
                raise SALMAException("Achieve-and-sustain goal {} was not contained in the evaluation "
                                     "step's top-level results ({})!".format(pname, toplevel_results))

            if World.__check_property_success(pname, toplevel_results, scheduled_results):
                self.__already_achieved_goals.add(pname)
            elif (World.__check_property_failure(pname, toplevel_results, scheduled_results)
                  and pname in self.__already_achieved_goals):
                verdict = NOT_OK
                failed_sustain_goals.add(pname)

        for pname in self.properties.keys():
            if pname in scheduled_keys:
                pending_properties.add(pname)

        # if there's no achieve goal, we actually just run until some time limit (see runExperiment)
        if (len(self.__achieve_goals) + len(self.__achieve_and_sustain_goals) > 0):
            all_achieved = True
        else:
            all_achieved = False
        # check whether all achieve goals have been achieved
        for pname in (self.__achieve_goals.keys() | self.__achieve_and_sustain_goals.keys()):
            if not pname in self.__already_achieved_goals:
                all_achieved = False
                break

        if (verdict == NONDET and all_achieved is True
            and len(pending_properties) == 0):
            verdict = OK

        return verdict, failed_invariants, failed_sustain_goals

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
        #: :type: dict[str, list[int]]
        scheduled_keys = dict()
        time_out = False

        while (not self.is_finished()) and (not check_verdict or verdict == NONDET):
            #self.__finished, overall_verdict, toplevel_results, scheduled_results, actions, []
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
                 "scheduled_keys": scheduled_keys
                }
        )

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


    def __reset_domainmap(self):
        self.__domainMap = dict()
        for entity in self.__entities.values():
            if isinstance(entity, Agent):
                self.__agents[entity.id] = entity
            if entity.sortName in self.__domainMap:
                self.__domainMap[entity.sortName].add(entity)
            else:
                self.__domainMap[entity.sortName] = set([entity])

    def reset(self):

        # re-init domains
        self.__reset_domainmap()
        self.initialize(sample_fluent_values=False, removeFormulas=False)

        self.__already_achieved_goals = set()
        #: :type agent: Agent
        for agent in self.getAgents():
            agent.restart()

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
                #moduleLogger.info("Trial #{} --> {}".format(trial_number, verdict))
                self.log_info("Trial #{} --> {}, steps = {}, time = {}".format(trial_number, verdict, res["steps"], res["time"]))
                #print("Trial #{} --> {}\n   Info: {}".format(trial_number, verdict, res))
            trial_number += 1

            if hypothesis_test is not None:
                accepted_hypothesis = hypothesis_test.check_hypothesis_accepted(conclusive_trial_count, failures)
                should_continue = accepted_hypothesis is None
            else:
                should_continue = conclusive_trial_count < number_of_repetitions

        self.reset()
        World.logic_engine().restoreState(current_state)
        return accepted_hypothesis, results, trial_infos

    def printState(self):
        if not self.__initialized: self.initialize()
        state = World.logic_engine().getCurrentState()
        for f in state:
            print(f)

    def log_info(self, msg):
        print("INFO: {}".format(msg))

    def describe_actions(self):
        actions = self.getAllActions()
        action_descriptions = []
        for a in actions:
            action_descriptions.append(a.describe())
        exoactions = self.get_exogenous_actions()
        for ea in exoactions:
            action_descriptions.append(ea.describe())
        return "\n".join(action_descriptions)

    def registerProperty(self, propertyName, formula, property_type):
        """
        Registers the given formula under the given name.
        :param str propertyName: the name of the property usd for referencing it later.
        :param str formula: the formula.
        :param int property_type: one of World.INVARIANT, World.ACHIEVE, or World.ACHIEVE_AND_SUSTAIN
        """
        if property_type not in (World.INVARIANT, World.ACHIEVE, World.ACHIEVE_AND_SUSTAIN):
            raise SALMAException("Unknown property type: {}".format(property_type))
        if (propertyName in self.__invariants or propertyName in self.__achieve_goals
            or propertyName in self.__achieve_and_sustain_goals):
            raise SALMAException("Property {} already registered.".format(propertyName))
        World.logic_engine().registerProperty(propertyName, formula)
        if property_type == World.INVARIANT:
            self.__invariants[propertyName] = (formula, property_type)
        elif property_type == World.ACHIEVE:
            self.__achieve_goals[propertyName] = (formula, property_type)
        else:
            self.__achieve_and_sustain_goals[propertyName] = (formula, property_type)

    def unregister_property(self, property_name):
        """
        Un-registers a property.
        :param str property_name: the property's name
        """
        if property_name in self.__invariants:
            del self.__invariants[property_name]
        if property_name in self.__achieve_goals:
            del self.__achieve_goals[property_name]
        if property_name in self.__achieve_and_sustain_goals:
            del self.__achieve_and_sustain_goals[property_name]

    @property
    def properties(self):
        """
        A new dict containing all properties that are registered currently. The dict has the
          structure name -> (formula, property_type)
        :rtype: dict[str, (str, int)]
        """
        all_props = dict(self.__invariants)
        all_props.update(self.__achieve_goals)
        all_props.update(self.__achieve_and_sustain_goals)
        return all_props

    @property
    def invariants(self):
        return self.__invariants

    @property
    def achieve_goals(self):
        return self.__achieve_goals

    @property
    def achieve_and_sustain_goals(self):
        return self.__achieve_and_sustain_goals

    def all_goals(self):
        goals = dict()
        goals.update(self.__achieve_goals)
        goals.update(self.__achieve_and_sustain_goals)
        return goals

    def getActionClock(self, actionName, params):
        """
        Returns the last recorded time when the given action was galled with the given parameters.

        If the given action-parameter combination hasn't occurred before, this method returns -1.

        :param actionName: str
        :param params: list

        :rtype: int
        """
        return self.logic_engine().getActionClock(actionName, params)

    def getFluentChangeTime(self, fluentName, params):
        """
        Returns the last recorded time when the given fluent with the given parameters.

        If the given fluent-parameter combination has not been initialized yet,
        this method returns -1.

        :param fluentName: str
        :param params: list
        """
        return self.logic_engine().getFluentChangeTime(fluentName, params)

    def queryPersistentProperty(self, propertyName):
        '''
        Returns a tuple with the current status of the given property together with the last
        change time.
        
        :param propertyName: str
        '''
        return self.logic_engine().queryPersistentProperty(propertyName)





# --------------------------------------------------------------------------------------

class LocalEvaluationContext(EvaluationContext):
    """
    An evaluation context that is coupled with an entity.
    """

    def __init__(self, contextEntity, parent):
        """
        :type contextEntity: Entity
        :type parent: EvaluationContext
        """
        EvaluationContext.__init__(self, parent)
        self.__contextEntity = contextEntity
        self.__variableBindings = dict()  # variable name
        self.__variableBindings[Entity.SELF] = self.__contextEntity

    def evaluate_python(self, source_type, source, *resolvedParams):
        result = None
        if source_type == EvaluationContext.PYTHON_EXPRESSION:
            ctx = World.instance().getExpressionContext().copy()
            ctx.update(self.__variableBindings)
            ctx['self'] = self.__contextEntity.id
            ctx['params'] = resolvedParams
            result = eval(source, ctx)
        elif source_type == EvaluationContext.PYTHON_FUNCTION:
            result = source(*resolvedParams)
        elif source_type == EvaluationContext.EXTENDED_PYTHON_FUNCTION:  # is python function
            ctx = World.instance().getExpressionContext().copy()
            ctx.update(self.__variableBindings)
            ctx['agent'] = self.__contextEntity  # don't call it self here to avoid confusion in function
            result = source(*resolvedParams, **ctx)
        return result

    def evaluateCondition(self, sourceType, source, *params):
        resolvedParams = self.resolve(*params)
        result = None
        if sourceType == EvaluationContext.FLUENT:
            result = World.instance().getFluentValue(source, resolvedParams)
            if result == None:
                raise SALMAException("No value found for fluent: {0}({1}).".format(source, resolvedParams))
        elif sourceType == EvaluationContext.ECLP_FUNCTION:
            result = World.logic_engine().evaluateCondition(source, *resolvedParams)
        elif sourceType == EvaluationContext.TRANSIENT_FLUENT:
            result = World.logic_engine().evaluateCondition(source, *resolvedParams, situation='s0')
        elif sourceType == EvaluationContext.CONSTANT:
            result = bool(World.getConstantValue(source, resolvedParams))
        elif sourceType in {EvaluationContext.PYTHON_EXPRESSION, EvaluationContext.PYTHON_FUNCTION,
                            EvaluationContext.EXTENDED_PYTHON_FUNCTION}:
            result = self.evaluate_python(sourceType, source, *resolvedParams)
        else:
            raise SALMAException("Unsupported source type: {}".format(sourceType))
        return result

    def evaluateFunction(self, sourceType, source, *params):
        resolvedParams = self.resolve(*params)
        result = None
        if sourceType == EvaluationContext.FLUENT:
            fv = World.instance().getFluentValue(source, resolvedParams)
            if fv is None:
                raise SALMAException("No value found for fluent: {0}({1}).".format(source, resolvedParams))
            result = fv
        elif sourceType == EvaluationContext.ECLP_FUNCTION:
            result = World.logic_engine().evaluateFunctionGoal(source, *resolvedParams)
        elif sourceType == EvaluationContext.TRANSIENT_FLUENT:
            result = World.logic_engine().evaluateFunctionGoal(source, *resolvedParams, situation='s0')
        elif sourceType == EvaluationContext.CONSTANT:
            result = World.instance.getConstantValue(source, resolvedParams)
        elif sourceType in {EvaluationContext.PYTHON_EXPRESSION, EvaluationContext.PYTHON_FUNCTION,
                            EvaluationContext.EXTENDED_PYTHON_FUNCTION}:
            result = self.evaluate_python(sourceType, source, *resolvedParams)
        else:
            raise SALMAException("Unsupported source type: {}".format(sourceType))

        if isinstance(result, str):
            result = self.getEntity(result)
        return result

    def getFluentValue(self, fluentName, *params):
        resolvedParams = self.resolve(*params)
        fv = World.instance().getFluentValue(fluentName, resolvedParams)
        if fv == None:
            raise SALMAException("No value found for fluent: {0}({1}).".format(fluentName, resolvedParams))
        return fv

    def set_fluent_value(self, fluent_name, params, value):
        resolved_params = self.resolve(*params)
        World.logic_engine().setFluentValue(fluent_name, params, value)

    def create_message(self, connector, agent, params):
        """
        Creates and returns a mew message for the given channel, the given agent, and the given parameters.
        A new "virtual" object of sort "message" is created and its id returned. The message
         is added to the domain of sort message. Additionally, the constant message_spec is set with the term
         msg(Con, Agent, Params).

        :type connector: str
        :type agent: str
        :type params: list
        :rtype: int
        """
        return World.logic_engine().create_message(connector, agent, params)

    def assignVariable(self, variableName, value):
        """
        :param str variableName: name of variable that should be set.
        :param object value: the value to assign
        """
        self.__variableBindings[variableName] = value

    def resolve(self, *terms):
        """
        Evaluates each term in terms and returns a list with the collected results.

        Entity instances are converted to their ids.
        """
        groundTerms = []
        for term in terms:
            gt = None
            if term is Entity.SELF:
                gt = self.__contextEntity.id
            elif isinstance(term, Variable):
                if not term.name in self.__variableBindings:
                    raise SALMAException("Variable %s not bound." % term.name)
                gt = self.__variableBindings[term.name]
            elif isinstance(term, (list, set)):
                gt = list()
                for t in term:
                    gt.append(self.resolve(t)[0])
            elif isinstance(term, tuple):
                l = list()
                for t in term:
                    l.append(self.resolve(t)[0])
                gt = tuple(l)
            else:
                gt = term

            if isinstance(gt, Entity):
                gt = gt.id

            groundTerms.append(gt)

        return groundTerms

    def getEntity(self, entityId):
        """
        returns the entity with the given id
        """
        return World.instance().getEntityById(entityId)

    def __select_free_variables(self, params):
        """
        :param list params: the parameters
        :rtype: list[(str,str)]
        """
        vars = []
        for p in params:
            if isinstance(p, tuple) and len(p) == 2 and isinstance(p[0], str) and isinstance(p[1], str):
                vars.append(p)
        return vars

    def selectAll(self, source_type, source, *params):
        """
        Returns a list of dicts with {paramName => Entity} entries that fulfill the given predicate with
        the given parameters.

        The parameter list can include ground values, bound variables and (name, sort) tuples.

        :param int source_type: the type of the predicate
        :param str source: the name of the predicate
        """

        resolved_params = self.resolve(*params)  # the free variables tuples are ignored by resolve()

        free_vars = self.__select_free_variables(params)
        if len(free_vars) == 0:
            raise SALMAException("No iterator variable specified in selectAll.")

        sit = 's0' if source_type in [EvaluationContext.FLUENT, EvaluationContext.TRANSIENT_FLUENT] else None
        result_list = []

        if source_type in {EvaluationContext.FLUENT, EvaluationContext.TRANSIENT_FLUENT,
                           EvaluationContext.ECLP_FUNCTION}:
            result_list = World.logic_engine().selectAll(source, *resolved_params, situation=sit)
        elif source_type in {EvaluationContext.PYTHON_EXPRESSION, EvaluationContext.PYTHON_FUNCTION,
                             EvaluationContext.EXTENDED_PYTHON_FUNCTION}:
            result_list = self.evaluate_python(source_type, source, *resolved_params)
        elif source_type == EvaluationContext.ITERATOR:
            # Here we just use a python object that supports the iterator protocol
            if isinstance(source, Variable):
                result_list = self.resolve(source)[0]
            else:
                result_list = source
            if not "__iter__" in result_list.__dir__():
                raise SALMAException("Trying to use non-iterator in Iterate statement: {} ".format(str(source)))
        else:
            raise SALMAException("Unsupported source type for Iterate statement: {}".format(source_type))

        refined_result = []

        #: :type result_entry: dict

        for result_entry in result_list:
            assignment = None
            refined_assignment = dict()
            if isinstance(result_entry, dict):
                assignment = result_entry
            else:
                if len(free_vars) == 1:
                    assignment = dict({free_vars[0][0]: result_entry})
                else:
                    if (not isinstance(result_entry, (list, tuple)) or
                                len(free_vars) != len(result_entry)):
                        raise SALMAException("Number of free variables in iterator doesn't match element structure.")
                    assignment = dict()
                    for fv, value in zip(free_vars, result_entry):
                        assignment[fv[0]] = value

            for name, value in assignment.items():
                #TODO: handle params with interval domains?
                if isinstance(value, str):
                    refined_assignment[name] = self.getEntity(value)
                else:
                    refined_assignment[name] = value

            refined_result.append(refined_assignment)

        return refined_result

    def selectFirst(self, predicateType, predicateName, *params):
        """
        Returns a dict with {paramName => Entity} entries that represents the first value combination
        that fulfills the given predicate with the given parameters.

        The parameter list can include ground values, bound variables and (name, sort) tuples.
        """
        resolvedParams = self.resolve(*params)  # the free variables tuples are ignored by resolve()

        sit = 's0' if predicateType in [EvaluationContext.FLUENT, EvaluationContext.TRANSIENT_FLUENT] else None

        result = World.logic_engine().selectFirst(predicateName, *resolvedParams, situation=sit)
        if result is None:
            return None

        refinedResult = dict()

        #: :type valueCombination: dict 

        for name, value in result.items():
            #TODO: handle params with interval domains?

            if isinstance(value, str):
                refinedResult[name] = self.getEntity(value)
            else:
                refinedResult[name] = value
        return refinedResult

    def createPlan(self, procedureName, *params):
        resolvedParams = self.resolve(*params)
        plan, values = World.logic_engine().createPlan(procedureName,
                                                       *resolvedParams)
        if values is None:
            return (None, None)

        refinedValues = dict()

        for name, value in values.items():
            if isinstance(value, str):
                refinedValues[name] = self.getEntity(value)
            else:
                refinedValues[name] = value

        refinedPlan = []
        for action in plan:
            actionName = action[0]
            params = action[1:]  # remember: this slicing will return  [] if there's no more than 1 elements in action
            refinedPlan.append(Act(actionName, params))

        return refinedPlan, refinedValues

    def createChildContext(self):
        return LocalEvaluationContext(self.__contextEntity, self)

    def getSorts(self):
        return World.instance().getSorts()

    def getDomain(self, sortName):
        return World.instance().getDomain(sortName)
    