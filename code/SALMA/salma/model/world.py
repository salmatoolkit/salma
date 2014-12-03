import itertools
import logging
import random

import pyclp

from salma.SALMAException import SALMAException
from salma.model.actions import StochasticAction, DeterministicAction, ExogenousAction, RandomActionOutcome, \
    Deterministic, Uniform
from salma.model.core import Constant, Action
from salma.model.evaluationcontext import EvaluationContext
from ..engine import Engine
from salma.model.eventschedule import EventSchedule
from salma.model.world_declaration import WorldDeclaration
from .core import Entity, Fluent
from .agent import Agent
from .procedure import Variable, Act
from salma.model.infotransfer import Connector, Channel, Sensor, RemoteSensor
from collections.abc import Iterable
from salma.mathutils import min_robust
from salma.termutils import tuplify

MODULE_LOGGER_NAME = 'salma.model'
moduleLogger = logging.getLogger(MODULE_LOGGER_NAME)


class World(Entity, WorldDeclaration):
    """
    Singleton class that acts as the *center of the simulation*. The domain model is loaded with load_declaration and
    all actions can be configured using the appropriate configuration classes. The configuration can then be checked
    with several check_* methods. After that, the domain models are set up with initialize, optionally also generating
    a randomized initial situation by sampling values for all fluent and constant instances. Additionally, the fluent
    and constant values can also be manipulated with setter and getter functions.
    """

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
        # : :type : dict[str, Fluent]
        self.__fluents = dict()

        # : :type: dict[str, (str, str, list)]
        self.__derived_fluents = dict()

        # : :type: dict[str, Constant]
        self.__constants = dict()

        # action_name -> core.Action
        # : :type : dict[str, Action]
        self.__actions = dict()

        self.__virtualSorts = {"sort", "message"}

        # store all entities in a sort -> entity dict
        # : :type: dict[str, set[Entity]]
        self.__domainMap = dict()
        # agents is a dict that stores
        # : :type: dict[str, Entity]
        self.__entities = dict()
        # : :type: dict[str, Agent]
        self.__agents = dict()

        # ------------------- event schedule ---------------------------
        # : :type: EventSchedule
        self.__event_schedule = EventSchedule(World.logic_engine())

        # ------------------- information transfer ---------------------
        #: :type: dict[str, Connector]
        self.__connectors = dict()

        # ------------------- information transfer end ---------------------

        self.__expressionContext = dict()

        self.__finished = False
        self.__initialized = False

        #: :type: dict[str, object]
        self.__additional_expression_context_globals = dict()

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

        for action in filter(lambda a: isinstance(a, StochasticAction), self.__actions.values()):
            assert isinstance(action, StochasticAction)
            problems = action.check(self.__actions)
            if len(problems) > 0:
                problematic_stochastic_actions.append((action, problems))

        problematic_exogenous_actions = self.__event_schedule.check_exogenous_action_initialization()
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
            return self.__evaluationContext.evaluateFunction(EvaluationContext.TRANSIENT_FLUENT, derived_fluent_name,
                                                             *params)
        return __f

    def __create_general_functions(self, expression_context: dict):
        # todo: include derived fluents in expression context?
        def __fcc(fluentName, *params):
            return self.getFluentChangeTime(fluentName, params)

        def __ac(actionName, *params):
            return self.getActionClock(actionName, params)

        def __oc(actionName, *params):
            return self.getActionClock(actionName, params) == self.getTime()

        # todo: add action count
        expression_context['fluentClock'] = __fcc
        expression_context['actionClock'] = __ac
        expression_context['occur'] = __oc

    def __create_expression_context(self):
        self.__expressionContext = dict()
        self.__expressionContext.update(self.__additional_expression_context_globals)
        # : :type fluent: Fluent
        for fluent in self.__fluents.values():
            self.__expressionContext[fluent.name] = self.__make_fluent_access_function(fluent.name)
        for df in self.__derived_fluents.values():
            self.__expressionContext[df[0]] = self.__make_derived_fluent_access_function(df[0])
        for con in self.__constants.values():
            self.__expressionContext[con.name] = self.__make_constant_access_function(con.name)

        self.__create_general_functions(self.__expressionContext)

        # add a "variable" for each entity to allow access without quotation marks
        for entity_id in self.__entities.keys():
            if not str(entity_id) in self.__expressionContext:
                self.__expressionContext[str(entity_id)] = str(entity_id)

    def sample_fluent_values(self):
        """
        Creates samples for all instances of all fluents except 'time'
        """
        for fluent in itertools.filterfalse(lambda f: f.name == 'time', self.__fluents.values()):
            self.__initialize_fluent(fluent)

    def __load_stochastic_action_declarations(self, declarations, immediate_action_names):
        """
        Loads the stochastic declarations and creates StochasticAction objects accordingly.
        :param list[(str, list[(str, str)], list[str])] declarations: the declarations as
                (name, params, outcome_names) tuples
        :param list[str] immediate_action_names: list of action names
        """
        for sa in declarations:
            immediate = sa[0] in immediate_action_names
            params = sa[1]
            outcome_names = sa[2]
            outcomes = []
            for o_name in outcome_names:
                outcome_action = self.getAction(o_name)
                assert isinstance(outcome_action, DeterministicAction)
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
        self.__connectors = dict()

        self.__event_schedule.clear()
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
            self.__event_schedule.add_exogenous_action(ExogenousAction(ea[0], ea[1], ea[2]))

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
            remote_sensor = RemoteSensor(rs[0], rs[1], rs[2], rs[3])
            self.add_connector(remote_sensor)
            self.addEntity(Entity(remote_sensor.name, "remoteSensor"))

    def sync_domains(self):
        self.__domainMap = dict()
        domain_map_from_engine = World.logic_engine().initSortHierarchy()

        for sort, domain in domain_map_from_engine.items():
            self.__domainMap[sort] = set()
            for entityId in domain:
                # var entity : Entity
                try:
                    entity = self.__entities[entityId]
                except KeyError:
                    # add a new entity if required
                    entity = Entity(entityId, sort)
                    self.__entities[entityId] = entity
                self.__domainMap[sort].add(entity)

    def initialize(self, sample_fluent_values=True, removeFormulas=True, deleteConstants=True):
        """
        1. Sets up domains, i.e. defines the sets of entity objects for each sort.

        2. Optionally sets ups the a new initial situation by creating samples for the fluent instance for
            each combination of parameter values.
        """
        World.logic_engine().reset(removeFormulas=removeFormulas, deleteConstants=deleteConstants)
        self.__evaluationContext = LocalEvaluationContext(self, None)

        for sort in self.__domainMap.keys():
            oids = []
            for entity in self.__domainMap[sort]:
                oids.append(entity.id)
            World.logic_engine().defineDomain(sort, oids)

        # keep entityId->entity maps (__entities & __agents)

        self.sync_domains()

        for chan in self.get_channels():
            self.setFluentValue("channel_in_queue", [chan.name], [])

        for rs in self.get_remote_sensors():
            self.setFluentValue("channel_in_queue", [rs.name], [])

        World.logic_engine().setFluentValue('time', [], 0)

        if sample_fluent_values:
            self.sample_fluent_values()

        self.__initialized = True
        # self.__persistentPropertiesInitialized = False
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
        if entityId not in self.__entities:
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
            self.__domainMap[entity.sortName] = {entity}

    def addAgent(self, agent):
        """
        Adds the given agent to the entity registry and set this world instance as the agent's evaluation context.
        :type agent: Agent
        """
        self.addEntity(agent)
        agent.evaluation_context = LocalEvaluationContext(agent, None)
        agent.world_declaration = self

    def removeEntity(self, entity):
        """
        Removes the given entity from the registry.
        :param entity: can either be an Entity or a string that specifies the id
        :type entity: Entity
        :raises: an SALMAException if the given entity was not found in the registry.
        """
        if entity.sortName not in self.__domainMap:
            raise SALMAException("Trying to remove entity {} with unregistered sort {}".format(
                entity.id, entity.sortName))
        eset = self.__domainMap[entity.sortName]
        if (entity not in eset) or (entity.id not in self.__entities):
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
        :rtype: Iterable[Action]
        """
        return self.__actions.values()

    def get_exogenous_action(self, action_name):
        """
        Returns the exogenous action with the given name.
        :raises: SALMAException if there is no exogenous action with the given name.
        :type action_name: str
        :rtype: ExogenousAction
        """
        try:
            return self.__event_schedule.exogenous_actions[action_name]
        except KeyError:
            raise (SALMAException("Unregistered exogenous action {}.".format(action_name)))

    def get_exogenous_actions(self):
        """
        Returns a list view on all exogenous actions.
        :rtype: Iterable[ExogenousAction]
        """
        return self.__event_schedule.exogenous_actions.values()

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
        :rtype: list[Entity]
        """
        try:
            return self.__domainMap[sort_name]
        except KeyError:
            raise (SALMAException("Sort {} not declared.".format(sort_name)))

    def addFluent(self, fluent):
        """
        Adds a core.Fluent object to the metamodel.
        NOTE: this method should normally not be called directly but is automatically called in
            World.load_declarations().
        :type fluent: Fluent
        """
        self.__fluents[fluent.name] = fluent

    def add_derived_fluent(self, fluent_name, fluent_type, params):
        self.__derived_fluents[fluent_name] = (fluent_name, fluent_type, params)

    def addConstant(self, con):
        """
        Adds a constant to the metamodel.
        NOTE: this method should normally not be called directly but is automatically
            called in World.load_declarations().
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
        :rtype: Iterable[Fluent]
        """
        return self.__fluents.values()

    def get_derived_fluents(self):
        """
        Returns the list of declared derived fluents as tuples of kind (fluent_name, fluent_type, params).
        :rtype: Iterable[(str, str, list)]
        """
        return self.__derived_fluents.values()

    def getConstants(self):
        """
        Returns a list of all registered constants.
        :rtype: Iterable[Constant]
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
        :return: the connector or None if no connector with the given name exists
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
        :type fluent_params: list|tuple
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
        :type constantParams: list|tuple
        :rtype: object
        """
        return World.logic_engine().getConstantValue(constantName, constantParams)

    def setFluentValue(self, fluentName, fluentParams, value):
        """
        Sets the current value for the given fluent instance.
        :type fluentName: str
        :type fluentParams: list|tuple
        :type value: object
        """
        World.logic_engine().setFluentValue(fluentName, fluentParams, value)

    def setConstantValue(self, constantName, constantParams, value):
        """
        Sets the current value for the given constant instance.
        :type constantName: str
        :type constantParams: list|tuple
        :type value: object
        """
        World.logic_engine().setConstantValue(constantName, constantParams, value)

    def __translate_action_execution(self, evaluation_context, action_execution):
        """
        Generates a ground instance of an action from the given Act as a
        (Action, [ground_params], EvaluationContext) tuple. The given evaluation context is used to resolve the action
        parameters.

        :raises: SALMAException if action is not registered.

        :param EvaluationContext evaluation_context: the evaluation context used for parameter resolution
        :param Act action_execution: the action execution control node
        :rtype: (Action, tuple, EvaluationContext)
        """
        try:
            action = self.__actions[action_execution.actionName]
            ground_params = tuplify(evaluation_context.resolve(*action_execution.actionParameters))
            return action, ground_params, evaluation_context
        except KeyError:
            raise (
                SALMAException("Trying to execute unregistered action: {}".format(
                    action_execution.actionName)))

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

    def __get_next_process_time(self):
        """
        Returns the next known time at which a process will be activated.
        :rtype: int
        """
        current_time = self.getTime()
        min_time = None
        for agent in self.__agents.values():
            for proc in agent.processes:
                ptime = proc.get_next_known_activation_time(current_time)
                min_time = min_robust([min_time, ptime])
        return min_time

    def get_next_stop_time(self, time_limit, consider_scanning_points):
        """
        Determines the next point in time at which the simulation must stop.

        :param int time_limit: the upper bound for the returned time that is used when
         either no point in time could be determined or the determined point is later than time_limit.
        :param bool consider_scanning_points: whether or not scanning points should be considered, i.e. points
            where events will become possible / schedulable.
        :rtype: int
        """
        t1 = self.__get_next_process_time()
        # var t2 : int
        if consider_scanning_points:
            t2 = self.__event_schedule.get_next_time_checkpoint()
        else:
            ev = self.__event_schedule.get_next_scheduled_event()
            t2 = ev[0] if ev is not None else None
        return min_robust([t1, t2, time_limit])

    def step(self, time_limit, evaluate_properties=True):
        """
        Performs one discrete time step for all agents and optionally performs the
        evaluation of all registered properties.

        :param int time_limit: the upper time limit until which the search for event occurrences is scanned
        :param bool evaluate_properties: if True, perform property evaluation
        :returns: (self.__finished, toplevel_results, scheduled_results, scheduled_keys, performed_actions,
                failed_actions, failure_stack)
        :rtype: (bool, dict[str, int], dict, dict[str, list[int]], list, list, list)
        """
        current_time = self.getTime()
        if moduleLogger.isEnabledFor(logging.DEBUG):
            moduleLogger.debug("T = %d", current_time)
        performed_actions = []
        failed_actions = []
        # update the actual event schedule using only the possibility and schedulability information at hand
        self.__event_schedule.update_event_schedule(current_time, self.__evaluationContext,
                                                    scan=False)
        next_stop_time = None
        while True:
            due_events = self.__event_schedule.get_due_events(current_time)
            pre_events = []
            interleaved_events = []
            # randomly split actions into one part that are performed before process flow and another
            # that is interleaved with actions
            for ev in due_events:
                if random.random() < 0.5:
                    pre_events.append(ev)
                else:
                    interleaved_events.append(ev)

            pa, fa = self.__event_schedule.progress_interleaved(self.__evaluationContext, pre_events)
            performed_actions.extend(pa)
            failed_actions.extend(fa)

            # : :type: list[process.Process]
            active_processes = []

            # The schedule trigger predicate is evaluated iteratively here. This allows processes to
            # become runnable as an effect by a previous immediate action by another process.
            self.__finished = True
            for agent in self.__agents.values():
                if not agent.is_finished():
                    self.__finished = False
                active_processes.extend(agent.update_schedule())
            actions = []
            for proc in active_processes:
                action_execution = proc.step()
                if action_execution is not None:
                    act = self.__translate_action_execution(proc.current_evaluation_context, action_execution)
                    actions.append(act)

            actions.extend(interleaved_events)
            pa, fa = self.__event_schedule.progress_interleaved(self.__evaluationContext, actions)
            performed_actions.extend(pa)
            failed_actions.extend(fa)
            # for scanning for possible / schedulable events, don't stop at previously calculated
            # scanning point because this point might have become invalid during the last progression
            next_stop_time = self.get_next_stop_time(time_limit, consider_scanning_points=False)
            self.__event_schedule.update_event_schedule(current_time, self.__evaluationContext,
                                                        scan=True, scan_start=current_time,
                                                        scan_time_limit=next_stop_time)

            # continue until no actions or events were performed during the iteration and
            # the no event is scheduled / possible / schedulable at the current point
            if len(due_events) == 0 and len(actions) == 0:
                # update the next stop time using all available information
                next_stop_time = self.get_next_stop_time(time_limit, consider_scanning_points=True)
                if next_stop_time > current_time:
                    break
        # now we know that next_stop_time is set up correctly
        if evaluate_properties:
            toplevel_results, scheduled_results, scheduled_keys, failure_stack = World.logic_engine().evaluationStep(
                interval_end=next_stop_time - 1)  # TODO: check if bound is right

            if moduleLogger.isEnabledFor(logging.DEBUG):
                moduleLogger.debug(("  toplevel_results: {}\n"
                                    "  scheduled_results: {}\n"
                                    "  scheduled_keys: {}").format(toplevel_results, scheduled_results, scheduled_keys))
        else:
            toplevel_results, scheduled_results, scheduled_keys = dict(), dict(), []
            failure_stack = []

        World.logic_engine().progress([('tick', [next_stop_time - current_time])])

        return (self.__finished, toplevel_results, scheduled_results, scheduled_keys, performed_actions,
                failed_actions, failure_stack)

    def __reset_domainmap(self):
        self.__domainMap = dict()
        for entity in self.__entities.values():
            if isinstance(entity, Agent):
                self.__agents[entity.id] = entity
            if entity.sortName in self.__domainMap:
                self.__domainMap[entity.sortName].add(entity)
            else:
                self.__domainMap[entity.sortName] = {entity}

    def reset(self):
        # re-init domains
        self.__reset_domainmap()
        self.initialize(sample_fluent_values=False, removeFormulas=False)

        # : :type agent: Agent
        for agent in self.getAgents():
            agent.restart()

    def restore_state(self, fluent_values):
        """
        Uses the given list of fluent values to update the world state.
        :param list[FluentValue] fluent_values: the fluent values that define the state
        """
        World.logic_engine().restore_state(fluent_values)

    def printState(self):
        if not self.__initialized:
            self.initialize()
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

    def queryPersistentProperty(self, property_name):
        """
        Returns a tuple with the current status of the given property together with the last
        change time.

        :param str property_name: the name of the property
        """
        return self.logic_engine().queryPersistentProperty(property_name)


# --------------------------------------------------------------------------------------

class LocalEvaluationContext(EvaluationContext):
    """
    An evaluation context that is coupled with an entity.
    """

    def __init__(self, contextEntity, parent):
        """
        :type contextEntity: Entity
        :type parent: EvaluationContext|None
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

    def evaluateCondition(self, source_type, source, *params):
        """
        Evaluates the given condition.

        :param int source_type: FLUENT, TRANSIENT_FLUENT, ECLP_FUNCTION or PYTHON_FUNCTION
        :param object source: a python function, the name of a fluent, or a pothon expression
        :param list params: parameters
        :returns: true if evaluation succeeded
        :rtype: bool
        """
        resolved_params = self.resolve(*params)
        # var result : bool
        if source_type == EvaluationContext.FLUENT:
            assert isinstance(source, str)
            result = World.instance().getFluentValue(source, resolved_params)
            if result is None:
                raise SALMAException("No value found for fluent: {0}({1}).".format(source, resolved_params))
        elif source_type == EvaluationContext.ECLP_FUNCTION:
            result = World.logic_engine().evaluateCondition(source, *resolved_params)
        elif source_type == EvaluationContext.TRANSIENT_FLUENT:
            result = World.logic_engine().evaluateCondition(source, *resolved_params, situation='s0')
        elif source_type == EvaluationContext.CONSTANT:
            result = bool(World.logic_engine().getConstantValue(source, resolved_params))
        elif source_type in {EvaluationContext.PYTHON_EXPRESSION, EvaluationContext.PYTHON_FUNCTION,
                             EvaluationContext.EXTENDED_PYTHON_FUNCTION}:
            result = self.evaluate_python(source_type, source, *resolved_params)
        else:
            raise SALMAException("Unsupported source type: {}".format(source_type))
        return result

    def evaluateFunction(self, source_type, source, *params):
        resolvedParams = self.resolve(*params)
        # var result : bool
        if source_type == EvaluationContext.FLUENT:
            fv = World.instance().getFluentValue(source, resolvedParams)
            if fv is None:
                raise SALMAException("No value found for fluent: {0}({1}).".format(source, resolvedParams))
            result = fv
        elif source_type == EvaluationContext.ECLP_FUNCTION:
            result = World.logic_engine().evaluateFunctionGoal(source, *resolvedParams)
        elif source_type == EvaluationContext.TRANSIENT_FLUENT:
            result = World.logic_engine().evaluateFunctionGoal(source, *resolvedParams, situation='s0')
        elif source_type == EvaluationContext.CONSTANT:
            result = World.instance.getConstantValue(source, resolvedParams)
        elif source_type in {EvaluationContext.PYTHON_EXPRESSION,
                             EvaluationContext.PYTHON_FUNCTION,
                             EvaluationContext.EXTENDED_PYTHON_FUNCTION}:
            result = self.evaluate_python(source_type, source, *resolvedParams)
        else:
            raise SALMAException("Unsupported source type: {}".format(source_type))

        if isinstance(result, str):
            result = self.getEntity(result)
        return result

    def getFluentValue(self, fluentName, *params):
        resolvedParams = self.resolve(*params)
        fv = World.instance().getFluentValue(fluentName, resolvedParams)
        if fv is None:
            raise SALMAException("No value found for fluent: {0}({1}).".format(fluentName, resolvedParams))
        return fv

    def set_fluent_value(self, fluent_name: str, params: list, value: object):
        """
        Sets the value of the given fluent instance.

        NOTE: params has to be resolved first!
        """
        World.logic_engine().setFluentValue(fluent_name, params, value)

    def create_message(self, connector, agent, msg_type, params):
        """
        See documentation in Engine.create_message .

        :type connector: str
        :type agent: str
        :type msg_type: str
        :type params: list
        :rtype: int
        """
        return World.logic_engine().create_message(connector, agent, msg_type, params)

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

        :param list terms: the unresolved terms.
        :rtype: list
        """
        ground_terms = []
        for term in terms:
            # var gt : object
            if term is Entity.SELF:
                gt = self.__contextEntity.id
            elif isinstance(term, Variable):
                if term.name not in self.__variableBindings:
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

            ground_terms.append(gt)

        return ground_terms

    def getEntity(self, entity_id):
        """
        returns the entity with the given id
        """
        return World.instance().getEntityById(entity_id)

    def __select_free_variables(self, params):
        """
        :param list|tuple params: the parameters
        :rtype: list[(str,str)]
        """
        free_vars = []
        for p in params:
            if isinstance(p, tuple) and len(p) == 2 and isinstance(p[0], str) and isinstance(p[1], str):
                free_vars.append(p)
        return free_vars

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
        # var result_list : list
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
            if "__iter__" not in result_list.__dir__():
                raise SALMAException("Trying to use non-iterator in Iterate statement: {} ".format(str(source)))
        else:
            raise SALMAException("Unsupported source type for Iterate statement: {}".format(source_type))

        refined_result = []
        # : :type result_entry: dict
        for result_entry in result_list:
            # var assignment : dict
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
                # TODO: handle params with interval domains?
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

        # : :type valueCombination: dict

        for name, value in result.items():
            # TODO: handle params with interval domains?

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
            return None, None

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
        """
        Returns a list with all entities for the given sort.
        :type sortName: str
        :rtype: list[Entity]
        """
        return World.instance().getDomain(sortName)

    def get_connector(self, name: str) -> Connector:
        """
        See documentation in World.get_connector
        """
        return World.instance().get_connector(name)

    def queryPersistentProperty(self, property_name):
        World.logic_engine().queryPersistentProperty(property_name)

    def getActionClock(self, action_name, *params):
        World.logic_engine().getActionClock(action_name, params)

    def getFluentChangeTime(self, fluent_name, *params):
        World.logic_engine().getFluentChangeTime(fluent_name, params)