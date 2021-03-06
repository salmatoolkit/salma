import itertools
import logging
import random
from collections.abc import Iterable, Iterator
import inspect

from salma.SALMAException import SALMAException
from salma.model.actions import StochasticAction, DeterministicAction, RandomActionOutcome
from salma.model.distributions import ConstantDistribution, Never
from salma.model.events import ExogenousAction, ExogenousActionChoice
from salma.model.core import Constant, Action, Entity, Fluent, DerivedFluent, translate_entities
from salma.model.data import Term
from salma.model.evaluationcontext import EvaluationContext
from ..engine import Engine
from salma.model.eventschedule import EventSchedule
from salma.model.world_declaration import WorldDeclaration
from .agent import Agent
from .procedure import Variable, Act
from salma.model.infotransfer import Connector, Channel, Sensor, RemoteSensor
from salma.mathutils import min_robust, max_robust
from salma.termutils import tuplify

logger = logging.getLogger(__name__)


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

    def __init__(self, erase_properties=False):
        Entity.__init__(self, World.ENTITY_ID, World.SORT_NAME)

        # fluentName -> core.Fluent
        #: :type : dict[str, Fluent]
        self.__fluents = dict()

        #: :type: dict[str, DerivedFluent]
        self.__derived_fluents = dict()

        self.__constants = dict()
        """:type : dict[str, Constant]"""

        # action_name -> core.Action
        # : :type : dict[str, Action]
        self.__actions = dict()

        self.__virtualSorts = {"sort", "message"}

        # store all entities in a sort -> entity dict
        # : :type: dict[str, set[Entity]]
        self.__domainMap = dict()

        self.__entities = dict()
        """:type : dict[str, Entity]"""
        self.__agents = dict()
        """:type : dict[str, Agent]"""

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

        #: :type: set[str]
        self.__clp_functions = set()

        if World.logic_engine() is None:
            raise SALMAException("Engine not set when creating world.")
        World.logic_engine().reset(erase_properties=erase_properties)
        self.addFluent(Fluent("time", "integer", []))
        self.evaluation_context = LocalEvaluationContext(self, None)

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

    @property
    def clp_functions(self):
        """
        :rtype: set[str]
        """
        return self.__clp_functions

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

    def register_clp_function(self, name):
        self.__clp_functions.add(name)

    @staticmethod
    def create_new_world(erase_properties=False):
        """
        Creates a new instance for the singleton World and returns it.
        :rtype: World
        """
        World.__instance = World(erase_properties=erase_properties)
        return World.__instance

    def enumerate_fluent_instances(self, fluent):
        """
        Creates an iterator for instances of the given fluent
        :type fluent: Fluent
        :rtype: list[list[str]]
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
            value = fluent.generateSample(self.evaluation_context, paramSelection)
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
                if not self.is_constant_defined(con.name, paramSelection):
                    instance = (con.name, paramSelection)
                    uninitialized_constant_instances.append(instance)

        for fluent in self.__fluents.values():
            """:type fluent: Fluent """
            for paramSelection in self.enumerate_fluent_instances(fluent):
                if not self.is_fluent__instance_defined(fluent.name, paramSelection):
                    instance = (fluent.name, paramSelection)
                    uninitialized_fluent_instances.append(instance)

        return uninitialized_fluent_instances, uninitialized_constant_instances

    def check_action_initialization(self):
        """
        Checks whether all stochastic and exogenous actions have been configured correctly.
        :return: tuple with (problematic_stochastic_actions, problematic_exogenous_actions) where each is a list of
        tuples with (action, promblem_list)

        :rtype: (list[(StochasticAction, list)], list[(ExogenousAction, list)], list[(ExogenousActionChoice, list)])
        """
        problematic_stochastic_actions = []

        for action in filter(lambda a: isinstance(a, StochasticAction), self.__actions.values()):
            assert isinstance(action, StochasticAction)
            problems = action.check(self.__actions)
            if len(problems) > 0:
                problematic_stochastic_actions.append((action, problems))

        problematic_exogenous_actions = self.__event_schedule.check_exogenous_action_initialization()
        problematic_exogenous_action_choices = self.__event_schedule.check_exogenous_action_choice_initialization()
        return problematic_stochastic_actions, problematic_exogenous_actions, problematic_exogenous_action_choices

    def __make_fluent_access_function(self, fluent_name):
        def __f(*params):
            return self.get_fluent_value(fluent_name, params)

        return __f

    def __make_constant_access_function(self, constant_name):
        def __f(*params):
            return self.getConstantValue(constant_name, params)

        return __f

    def __make_derived_fluent_access_function(self, derived_fluent_name):
        def __f(*params):
            return self.get_derived_fluent_value(derived_fluent_name, params)

        return __f

    def __create_general_functions(self, expression_context: dict):
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
        for fluent in self.__fluents.values():
            self.__expressionContext[fluent.name] = self.__make_fluent_access_function(fluent.name)
        for df in self.__derived_fluents.values():
            self.__expressionContext[df.name] = self.__make_derived_fluent_access_function(df.name)
        for con in self.__constants.values():
            self.__expressionContext[con.name] = self.__make_constant_access_function(con.name)

        self.__create_general_functions(self.__expressionContext)

        # add a "variable" for each entity to allow access without quotation marks
        for entity_id in self.__entities.keys():
            if not str(entity_id) in self.__expressionContext:
                self.__expressionContext[str(entity_id)] = self.__entities[entity_id]

    def sample_fluent_values(self):
        """
        Creates samples for all instances of all fluents except 'time'
        """
        for fluent in itertools.filterfalse(lambda f: f.name == 'time', self.__fluents.values()):
            self.__initialize_fluent(fluent)

    def __load_stochastic_action_declarations(self, declarations):
        """
        Loads the stochastic declarations and creates StochasticAction objects accordingly.
        :param list[(str, list[(str, str)], list[str])] declarations: the declarations as
                (name, params, outcome_names) tuples
        """
        for sa in declarations:
            params = sa[1]
            outcome_names = sa[2]
            #: :type: list[RandomActionOutcome]
            outcomes = []
            for o_name in outcome_names:
                outcome_action = self.getAction(o_name)
                assert isinstance(outcome_action, DeterministicAction)
                outcomes.append(RandomActionOutcome(outcome_action))
            action = StochasticAction(sa[0], params, outcomes, selection_strategy=None)
            self.addAction(action)

    def __load_exogenous_action_choice(self, declarations):
        """
        Loads the exogenous action choice declarations.
        :param list[(str, list[(str, str)], list[str], str, bool)] declarations: the declarations as
                (name, params, option_names, type, time-dependent) tuples.
        """
        for ec in declarations:
            choice_name, params, option_names, _, _ = ec
            #: :type: list[ExogenousAction]
            options = []
            for oname in option_names:
                try:
                    option_event = self.__event_schedule.exogenous_actions[oname]
                except KeyError:
                    raise SALMAException("Option '{}' of choice '{}' points to "
                                         "unregistered exogenous action".format(oname, choice_name))
                options.append(option_event)

            choice = ExogenousActionChoice(choice_name, params, options)
            self.__event_schedule.add_exogenous_action_choice(choice)

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
            self.addAction(DeterministicAction(pa[0], pa[1], pa[2]))

        self.__load_stochastic_action_declarations(declarations["stochastic_actions"])

        for ea in declarations['exogenous_actions']:
            self.__event_schedule.add_exogenous_action(ExogenousAction(ea[0], ea[1], ea[2], ea[3]))

        self.__load_exogenous_action_choice(declarations['exogenous_action_choices'])

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

    def __create_entity_accessors(self):
        for e in self.__entities.values():
            for fl in self.__fluents.values():
                if len(fl.parameters) > 0 and e in self.getDomain(fl.parameters[0][1]):
                    e.register_own_fluent(fl)
            for dfl in self.__derived_fluents.values():
                if len(dfl.parameters) > 0 and e in self.getDomain(dfl.parameters[0][1]):
                    e.register_own_derived_fluent(dfl)
            for c in self.__constants.values():
                if len(c.parameters) > 0 and e in self.getDomain(c.parameters[0][1]):
                    e.register_own_constant(c)

        # add features that have no parameters to the world
        for fl in self.__fluents.values():
            if len(fl.parameters) == 0:
                self.register_own_fluent(fl)
        for c in self.__constants.values():
            if len(c.parameters) == 0:
                self.register_own_constant(c)
        for dfl in self.__derived_fluents.values():
            if len(dfl.parameters) == 0:
                self.register_own_derived_fluent(dfl)

    def initialize(self, sample_fluent_values=False):
        """
        1. Sets up domains, i.e. defines the sets of entity objects for each sort.

        2. Optionally sets ups the a new initial situation by creating samples for the fluent instance for
            each combination of parameter values.
        """
        World.logic_engine().reset(erase_properties=False)
        self.evaluation_context = LocalEvaluationContext(self, None)

        for sort in self.__domainMap.keys():
            oids = []
            for entity in self.__domainMap[sort]:
                oids.append(entity.id)
            World.logic_engine().defineDomain(sort, oids)

        # keep entityId->entity maps (__entities & __agents)

        self.sync_domains()
        self.__create_entity_accessors()

        for chan in self.get_channels():
            self.set_fluent_value("channel_in_queue", [chan.name], [])

        for rs in self.get_remote_sensors():
            self.set_fluent_value("channel_in_queue", [rs.name], [])

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
        :rtype: list[Entity]
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

    def get_entities_by_id(self, *ids):
        """
        Returns a list of entities with the given ids. Raises SALMAException if an id
        is not registered.

        :param list[str] ids: the entity ids
        :rtype: list[Entity]
        """
        entities = []
        for i in ids:
            try:
                e = self.__entities[i]
                entities.append(e)
            except KeyError:
                raise SALMAException("No entity registered with id {}.".format(i))
        return entities

    def addEntity(self, entity):
        """
        Adds the given entity to the registry
        :type entity: Entity
        """
        if entity.id in self.__entities:
            raise SALMAException("Trying to register an entity whose id is already used: {}".format(entity.id))
        if not isinstance(entity, Entity):
            raise SALMAException("Trying to register an entity object that is not an instance of Entity: {}".format(
                str(entity)))
        self.__entities[entity.id] = entity
        if isinstance(entity, Agent):
            self.__agents[entity.id] = entity
        if entity.sortName in self.__domainMap:
            self.__domainMap[entity.sortName].add(entity)
        else:
            self.__domainMap[entity.sortName] = {entity}
        entity.evaluation_context = LocalEvaluationContext(entity, None)

    def addAgent(self, agent):
        """
        Adds the given agent to the entity registry and set this world instance as the agent's evaluation context.
        :type agent: Agent
        """
        if not isinstance(agent, Agent):
            raise SALMAException("Trying to register an agent object that is not an instance of Agent: {}".format(
                str(agent)))
        self.addEntity(agent)
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

    def get_exogenous_action_choice(self, choice_name):
        """
        Return the exogenous action choice with the given name.
        :param str choice_name: name of the choice
        :rtype: ExogenousActionChoice
        """
        try:
            return self.__event_schedule.exogenous_action_choices[choice_name]
        except KeyError:
            raise (SALMAException("Unregistered exogenous action choice {}.".format(choice_name)))

    def get_exogenous_action_choices(self):
        """
        Return a list view on all registered exogenous action choices.
        :rtype: Iterable[ExogenousActionChoice]
        """
        return self.__event_schedule.exogenous_action_choices.values()

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
        self.__derived_fluents[fluent_name] = DerivedFluent(fluent_name, fluent_type, params)

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

    def __add_anything(self, item):
        """
        Adds an entities, agents, etc. to the world
        """
        if isinstance(item, Agent):
            self.addAgent(item)
        elif isinstance(item, Entity) and not isinstance(item, Agent):
            self.addEntity(item)

    def add(self, *items):
        """
        Adds one or more entities, agents, etc. to the world
        :param list items: the items to add
        """
        for item in items:
            self.__add_anything(item)

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
        :rtype: Iterable[DerivedFluent]
        """
        return self.__derived_fluents.values()

    def get_derived_fluent(self, derived_fluent_name):
        if derived_fluent_name in self.__derived_fluents:
            return self.__derived_fluents[derived_fluent_name]
        else:
            return None

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

    def deactivate_info_transfer(self):
        """
        Deactivates all exogenous actions for information transfer.
        """
        for ea_name in ["transferStarts", "transferEnds"]:
            ea = self.get_exogenous_action(ea_name)
            ea.config.occurrence_distribution = Never()
            ea.config.set_param_distribution("error", ConstantDistribution("term", None))

        ea = self.get_exogenous_action("transferFails")
        ea.config.occurrence_distribution = Never()

    def deactivate_all_events(self):
        """
        Deactivates all events by setting their occurrence distributions to NEVER or DONT_OCCUR.
        """
        self.__event_schedule.deactivate_all_events()

    @staticmethod
    def instance():
        """
        Returns the singleton instance of the World.
        :rtype: World
        """
        if World.__instance is None:
            World.__instance = World()
        return World.__instance

    def lookup_entities(self, term):
        """
        Replaces every entity id string in the given "term" by its entity object.
        :param term: the term to translate
        """
        if term is None:
            return None
        elif isinstance(term, Entity):
            return term
        elif isinstance(term, str):
            return self.getEntityById(term)
        elif isinstance(term, (list, tuple, set)):
            resolved = []
            for t in term:
                resolved.append(self.lookup_entities(t))
            if isinstance(term, tuple):
                return tuple(resolved)
            elif isinstance(term, set):
                return set(resolved)
            else:
                return resolved
        elif isinstance(term, dict):
            # resolve only values
            resolved = dict()
            for k, v in term.items():
                resolved[k] = self.lookup_entities(v)
            return resolved
        else:
            return term

    # noinspection PyMethodMayBeStatic
    def get_fluent_value(self, fluent_name, fluent_params):
        """
        Returns the current value of the fluent instance that is given by the fluent name and the parameter list.
        If the fluent instance is undefined, this method returns None.
        :type fluent_name: str
        :type fluent_params: list|tuple
        :rtype: object
        """
        # we don't check if the fluent has been registered for performance reasons
        fv = World.logic_engine().getFluentValue(fluent_name, *translate_entities(fluent_params))
        if fv is None:
            return None
        else:
            return self.lookup_entities(fv.value)

    # noinspection PyMethodMayBeStatic
    def get_state_snapshot(self):
        """
        Returns a state snapshot that contains all fluent instances except the domains.
        :rtype: list[FluentValue]
        """
        current_state = [fv for fv in World.logic_engine().getCurrentState() if fv.fluentName != "domain"]
        return current_state

    # noinspection PyMethodMayBeStatic
    def get_derived_fluent_value(self, fluent_name, fluent_params):
        """
        Retrieves the current value of the given derived fluent instance.
        :param str fluent_name: the name of the derived fluent.
        :param list|tuple fluent_params: the parameters defining the derived fluent instance.
        :rtype: object
        """
        val = World.logic_engine().get_derived_fluent_value(fluent_name, translate_entities(fluent_params))
        return self.lookup_entities(val)

    # noinspection PyMethodMayBeStatic
    def is_fluent__instance_defined(self, fluent_name, fluent_params):
        """
        Returns True if the given fluent instance defined.
        :param str fluent_name:
        :param list|tuple fluent_params: the parameters defining the fluent instance
        :rtype: bool
        """
        fv = World.logic_engine().getFluentValue(fluent_name, *translate_entities(fluent_params))
        return fv is not None

    def getTime(self):
        """
        Returns the current value of the fluent 'time'.
        :rtype: int
        """
        return self.get_fluent_value('time', [])

    # noinspection PyMethodMayBeStatic
    def getConstantValue(self, constantName, constantParams):
        """
        Returns the current value of the given constant instance.
        :type constantName: str
        :type constantParams: list|tuple
        :rtype: object
        """
        cv = World.logic_engine().getConstantValue(constantName, translate_entities(constantParams))
        if cv is None:
            return None
        else:
            return self.lookup_entities(cv.value)

    # noinspection PyMethodMayBeStatic
    def is_constant_defined(self, constant_name, constant_params):
        """
        Returns True if the given constant is defined.
        :param str constant_name:
        :param list|tuple constant_params: the parameters
        :rtype: bool
        """
        return World.logic_engine().isConstantDefined(constant_name, translate_entities(constant_params))

    # noinspection PyMethodMayBeStatic
    def set_fluent_value(self, fluent_name, fluent_params, value):
        """
        Sets the current value for the given fluent instance.
        :type fluent_name: str
        :type fluent_params: list|tuple
        :type value: object
        """
        World.logic_engine().setFluentValue(fluent_name, translate_entities(fluent_params), value)

    # noinspection PyMethodMayBeStatic
    def set_constant_value(self, constant_name, constant_params, value):
        """
        Sets the current value for the given constant instance.
        :type constant_name: str
        :type constant_params: list|tuple
        :type value: object
        """
        World.logic_engine().setConstantValue(constant_name, translate_entities(constant_params), value)

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
            # note that we don't allow a variable for the action name!
            action = self.__actions[action_execution.action_name]

            ground_params = tuplify(evaluation_context.resolve(*action_execution.action_parameters))
            return action, ground_params, evaluation_context
        except KeyError:
            raise (
                SALMAException("Trying to execute unregistered action: {}".format(
                    action_execution.action_name)))

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

    def get_next_stop_time(self, consider_scanning_points):
        """
        Determines the next point in time at which the simulation must stop.

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
            t2 = ev.time_point if ev is not None else None
        return min_robust([t1, t2])

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
        if logger.isEnabledFor(logging.DEBUG):
            logger.debug("T = %d", current_time)
        performed_actions = []
        failed_actions = []
        # update the actual event schedule using only the possibility and schedulability information at hand
        self.__event_schedule.update_event_schedule(current_time, self.evaluation_context,
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

            pa, fa = self.__event_schedule.progress_interleaved(self.evaluation_context, pre_events)
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
            random.shuffle(active_processes)
            actions = []
            atomic_actions = []
            for proc in active_processes:
                action_execution = proc.step()
                if action_execution is not None:
                    action, params, ec = self.__translate_action_execution(proc.current_evaluation_context,
                                                                           action_execution)
                    # if the action is atomic, don't interleave but progress now
                    if isinstance(action, DeterministicAction) and action.atomic:
                        atomic_action = (action, params, ec)
                        atomic_actions.append(atomic_action)
                        pa, fa = self.__event_schedule.progress_interleaved(self.evaluation_context, [atomic_action])
                        performed_actions.extend(pa)
                        failed_actions.extend(fa)
                    else:
                        actions.append((action, params, ec))

            actions.extend(interleaved_events)
            pa, fa = self.__event_schedule.progress_interleaved(self.evaluation_context, actions)
            performed_actions.extend(pa)
            failed_actions.extend(fa)
            # for scanning for possible / schedulable events, don't stop at previously calculated
            # scanning point because this point might have become invalid during the last progression
            next_stop_time = self.get_next_stop_time(consider_scanning_points=False)

            stl = min_robust([next_stop_time, time_limit])
            assert isinstance(stl, int)
            self.__event_schedule.update_event_schedule(current_time, self.evaluation_context,
                                                        scan=True, scan_start=current_time,
                                                        scan_time_limit=stl)

            # continue until no actions or events were performed during the iteration and
            # the no event is scheduled / possible / schedulable at the current point
            if len(due_events) == 0 and len(actions) == 0 and len(atomic_actions) == 0:
                # update the next stop time using all available information
                next_stop_time = self.get_next_stop_time(consider_scanning_points=True)
                if next_stop_time is None or next_stop_time > current_time:
                    break
        if self.is_finished():
            interval_end = current_time
        else:
            if next_stop_time is None:
                interval_end = min_robust([current_time, time_limit])
            else:
                interval_end = min_robust([next_stop_time - 1, time_limit])
        assert isinstance(interval_end, int)
        # now we know that next_stop_time is set up correctly
        if evaluate_properties:
            ie2 = max_robust([interval_end, current_time])
            assert isinstance(ie2, int)
            toplevel_results, scheduled_results, scheduled_keys, failure_stack = World.logic_engine().evaluationStep(
                interval_end=ie2)
            if logger.isEnabledFor(logging.DEBUG):
                logger.debug(("  toplevel_results: {}\n"
                              "  scheduled_results: {}\n"
                              "  scheduled_keys: {}").format(toplevel_results,
                                                             scheduled_results,
                                                             scheduled_keys))
        else:
            toplevel_results, scheduled_results, scheduled_keys = dict(), dict(), []
            failure_stack = []
        World.logic_engine().progress([('tick', [interval_end - current_time + 1])])
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
        self.initialize()

        # : :type agent: Agent
        for agent in self.getAgents():
            agent.restart()

    # noinspection PyMethodMayBeStatic
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

    # noinspection PyMethodMayBeStatic
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
        return self.logic_engine().getActionClock(actionName, translate_entities(params))

    def getFluentChangeTime(self, fluentName, params):
        """
        Returns the last recorded time when the given fluent with the given parameters.

        If the given fluent-parameter combination has not been initialized yet,
        this method returns -1.

        :param fluentName: str
        :param params: list
        """
        return self.logic_engine().getFluentChangeTime(fluentName, translate_entities(params))

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

    def __init__(self, context_entity, parent):
        """
        :type context_entity: Entity
        :type parent: EvaluationContext|None
        """
        EvaluationContext.__init__(self, parent)
        self.__context_entity = context_entity
        self.variable_bindings[Entity.SELF] = self.__context_entity

    def evaluate_python(self, source_type, source, *resolved_params):
        result = None
        if source_type == EvaluationContext.PYTHON_EXPRESSION:
            source = str(source)
            ctx = World.instance().getExpressionContext().copy()
            ctx.update(self.resolve(self.global_variable_bindings, resolve_entities=False)[0])
            ctx.update(self.resolve(self.variable_bindings, resolve_entities=False)[0])

            ctx['self'] = self.__context_entity
            ctx['params'] = resolved_params
            result = eval(source, ctx)
        elif source_type == EvaluationContext.PYTHON_FUNCTION:
            result = source(*resolved_params)
        elif source_type == EvaluationContext.EXTENDED_PYTHON_FUNCTION:  # is python function
            ctx = World.instance().getExpressionContext().copy()
            ctx.update(self.resolve(self.variable_bindings, resolve_entities=False)[0])
            ctx['agent'] = self.__context_entity  # don't call it self here to avoid confusion in function
            ctx['ctx'] = self
            result = source(*resolved_params, **ctx)
        return result

    def evaluateCondition(self, source_type, source, *params):
        """
        Evaluates the given condition.

        :param int source_type: FLUENT, TRANSIENT_FLUENT, ECLP_FUNCTION or PYTHON_FUNCTION
        :param object source: a python function, the name of a fluent, or a python expression
        :param list params: parameters
        :returns: true if evaluation succeeded
        :rtype: bool
        """
        resolve_entities = source_type not in (EvaluationContext.PYTHON_EXPRESSION, EvaluationContext.PYTHON_FUNCTION,
                                               EvaluationContext.EXTENDED_PYTHON_FUNCTION)
        resolved_params = self.resolve(*params, resolve_entities=resolve_entities)

        # var result : bool
        if source_type == EvaluationContext.FLUENT:
            assert isinstance(source, str)
            result = World.instance().get_fluent_value(source, resolved_params)
            if result is None:
                raise SALMAException("No value found for fluent: {0}({1}).".format(source, resolved_params))
        elif source_type == EvaluationContext.ECLP_FUNCTION:
            result = World.logic_engine().evaluateCondition(source, *resolved_params)
        elif source_type == EvaluationContext.TRANSIENT_FLUENT:
            result = World.logic_engine().evaluateCondition(source, *resolved_params, situation='s0')
        elif source_type == EvaluationContext.CONSTANT:
            result = bool(World.instance().getConstantValue(source, resolved_params))
        elif source_type in {EvaluationContext.PYTHON_EXPRESSION, EvaluationContext.PYTHON_FUNCTION,
                             EvaluationContext.EXTENDED_PYTHON_FUNCTION}:
            result = self.evaluate_python(source_type, source, *resolved_params)
        else:
            raise SALMAException("Unsupported source type: {}".format(source_type))
        return result

    def evaluateFunction(self, source_type, source, *params):
        resolve_entities = source_type not in (EvaluationContext.PYTHON_EXPRESSION, EvaluationContext.PYTHON_FUNCTION,
                                               EvaluationContext.EXTENDED_PYTHON_FUNCTION)
        resolved_params = self.resolve(*params, resolve_entities=resolve_entities)
        # var result : bool
        if source_type == EvaluationContext.FLUENT:
            fv = World.instance().get_fluent_value(source, resolved_params)
            if fv is None:
                raise SALMAException("No value found for fluent: {0}({1}).".format(source, resolved_params))
            result = fv
        elif source_type == EvaluationContext.ECLP_FUNCTION:
            result = World.logic_engine().evaluateFunctionGoal(source, *resolved_params)
        elif source_type == EvaluationContext.TRANSIENT_FLUENT:
            result = World.logic_engine().evaluateFunctionGoal(source, *resolved_params, situation='s0')
        elif source_type == EvaluationContext.CONSTANT:
            result = World.instance().getConstantValue(source, resolved_params)
        elif source_type in {EvaluationContext.PYTHON_EXPRESSION,
                             EvaluationContext.PYTHON_FUNCTION,
                             EvaluationContext.EXTENDED_PYTHON_FUNCTION}:
            result = self.evaluate_python(source_type, source, *resolved_params)
        else:
            raise SALMAException("Unsupported source type: {}".format(source_type))

        if isinstance(result, str):
            result = self.getEntity(result)
        return result

    def get_fluent_value(self, fluent_name, *params):
        resolved_params = self.resolve(*params)
        return World.instance().get_fluent_value(fluent_name, resolved_params)

    def get_current_time(self):
        return World.instance().get_fluent_value("time", [])

    def set_fluent_value(self, fluent_name: str, params: list, value: object):
        """
        Sets the value of the given fluent instance.
        """
        resolved_params = self.resolve(*params)
        resolved_value = self.resolve(value)[0]
        World.instance().set_fluent_value(fluent_name, resolved_params, resolved_value)

    def get_derived_fluent_value(self, fluent_name, params):
        resolved_params = self.resolve(*params)
        return World.instance().get_derived_fluent_value(fluent_name, resolved_params)

    def get_constant_value(self, constant_name, params):
        resolved_params = self.resolve(*params)
        return World.instance().getConstantValue(constant_name, resolved_params)

    def set_constant_value(self, constant_name, params, value):
        resolved_params = self.resolve(*params)
        resolved_value = self.resolve(value)[0]
        World.instance().set_constant_value(constant_name, resolved_params, resolved_value)

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

    def resolve(self, *terms, **kwargs):
        """
        Evaluates each term in terms and returns a list with the collected results.

        Entity instances are converted to their ids.

        :param list terms: the unresolved terms.
        :rtype: list
        """
        if "strict" in kwargs:
            strict = kwargs["strict"]
        else:
            strict = True

        if "resolve_entities" in kwargs:
            resolve_entities = kwargs["resolve_entities"]
        else:
            resolve_entities = True

        ground_terms = []
        for term in terms:
            # var gt : object
            if term is Entity.SELF:
                gt = self.__context_entity.id
            elif isinstance(term, Variable):
                if term.name not in self.variable_bindings:
                    if term.name not in self.global_variable_bindings:
                        if strict:
                            raise SALMAException("Variable %s not bound." % term.name)
                        else:
                            gt = (term.name, term.sort)
                    else:
                        gt = self.global_variable_bindings[term.name]
                else:
                    gt = self.variable_bindings[term.name]
            elif isinstance(term, (list, set)):
                gt = list()
                for t in term:
                    gt.append(self.resolve(t, **kwargs)[0])
            elif isinstance(term, tuple):
                l = list()
                for t in term:
                    l.append(self.resolve(t, **kwargs)[0])
                gt = tuple(l)
            elif isinstance(term, dict):
                gt = dict()
                for k, v in term.items():
                    gt[k] = self.resolve(v, **kwargs)[0]
            elif isinstance(term, Term):
                l = []
                for t in term.params:
                    l.append(self.resolve(t, **kwargs)[0])
                gt = Term(term.functor, *l)
            else:
                gt = term

            if isinstance(gt, Entity) and resolve_entities:
                gt = gt.id

            ground_terms.append(gt)

        return ground_terms

    def lookup_entities(self, term):
        """
        Replaces every entity id string in the given "term" by its entity object.
        :param term: the term to translate
        """
        return World.instance().lookup_entities(term)

    def getEntity(self, entity_id):
        """
        returns the entity with the given id
        """
        return World.instance().getEntityById(entity_id)

    # noinspection PyMethodMayBeStatic
    def __select_free_variables(self, params):
        """
        :param list|tuple params: the parameters
        :rtype: list[(str,str)]
        """
        free_vars = []
        for p in params:
            if isinstance(p, Variable):
                if p.sort is None:
                    raise SALMAException("No sort defined for free variable {} in Select.".format(p.name))
                free_vars.append((p.name, p.sort))
        return free_vars

    def selectAll(self, source_type, source, *params):
        """
        Returns a list of dicts with {paramName => Entity} entries that fulfill the given predicate with
        the given parameters.

        The parameter list can include ground values, bound variables and (name, sort) tuples.

        :param int source_type: the type of the predicate
        :param str source: the name of the predicate
        """
        free_vars = self.__select_free_variables(params)
        resolve_entities = source_type not in (EvaluationContext.PYTHON_EXPRESSION, EvaluationContext.PYTHON_FUNCTION,
                                               EvaluationContext.EXTENDED_PYTHON_FUNCTION, EvaluationContext.ITERATOR)
        resolved_params = self.resolve(*params, strict=False,
                                       resolve_entities=resolve_entities)
        # the free variables tuples are ignored by resolve()

        if len(free_vars) == 0:
            raise SALMAException("No iterator variable specified in selectAll.")

        sit = 's0' if source_type in [EvaluationContext.FLUENT, EvaluationContext.TRANSIENT_FLUENT] else None
        # var result_list : list
        if source_type in {EvaluationContext.FLUENT, EvaluationContext.TRANSIENT_FLUENT,
                           EvaluationContext.ECLP_FUNCTION}:
            result_list = World.logic_engine().selectAll(source, *resolved_params, situation=sit)
        elif source_type in {EvaluationContext.PYTHON_EXPRESSION, EvaluationContext.PYTHON_FUNCTION,
                             EvaluationContext.EXTENDED_PYTHON_FUNCTION}:
            # for Python calls, we don't pass the free variables since the result will be extracted purely
            # from the returned value
            bound_params = [v for v in resolved_params if v not in free_vars]
            result_list = self.evaluate_python(source_type, source, *bound_params)
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
        freevars = self.__select_free_variables(params)

        resolvedParams = self.resolve(*params, strict=False)  # the free variables tuples are ignored by resolve()

        sit = 's0' if predicateType in [EvaluationContext.FLUENT, EvaluationContext.TRANSIENT_FLUENT] else None

        refinedResult = dict()
        result = World.logic_engine().selectFirst(predicateName, *resolvedParams, situation=sit)
        if result is None:
            for fv in freevars:
                refinedResult[fv[0]] = None
            return refinedResult

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

    def create_child_context(self):
        ec = LocalEvaluationContext(self.__context_entity, self)
        ec.set_agent(self.get_agent())
        ec.set_process(self.get_process())
        return ec

    def getSorts(self):
        return World.instance().getSorts()

    # noinspection PyUnusedLocal
    def determine_source_type(self, source, params):
        if isinstance(source, (Iterable, Iterator)) and not isinstance(source, str):
            return EvaluationContext.ITERATOR
        elif callable(source):
            if inspect.isfunction(source) and inspect.getfullargspec(source).varkw is not None:
                return EvaluationContext.EXTENDED_PYTHON_FUNCTION
            else:
                return EvaluationContext.PYTHON_FUNCTION
        else:
            source = str(source)
            wd = World.instance()
            if wd.getFluent(source) is not None:
                return EvaluationContext.FLUENT
            elif wd.getConstant(source) is not None:
                return EvaluationContext.CONSTANT
            elif wd.get_derived_fluent(source) is not None:
                return EvaluationContext.TRANSIENT_FLUENT
            elif source in wd.clp_functions:
                return EvaluationContext.ECLP_FUNCTION
            else:
                return EvaluationContext.PYTHON_EXPRESSION

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
