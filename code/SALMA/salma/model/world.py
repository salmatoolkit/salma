'''
Created on 21.05.2013

@author: christian
'''
import itertools
import logging
import random
import sys
import datetime
import time
import pyclp

from salma import constants
from salma.SMCException import SMCException
from salma.constants import *
from salma.model.actions import StochasticAction, DeterministicAction, ExogenousAction, RandomActionOutcome, Deterministic, Uniform, ExogenousActionConfiguration
from salma.model.core import Entity, Agent, Constant, Action
from salma.model.evaluationcontext import EvaluationContext
from salma.model.procedure import ActionExecution
from ..engine import Engine
from ..statistics import SequentialAcceptanceTest
from .core import Agent, Entity, Fluent
from .procedure import Variable, ActionExecution


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
        # action_name -> core.Action
        self.__actions = dict()

        self.__exogenousActions = dict()

        # store all entities in a sort -> entity dict 
        self.__domainMap = dict()
        # agents is a dict that stores
        self.__entities = dict()
        self.__agents = dict()

        self.__expressionContext = dict()

        self.__finished = False
        self.__initialized = False

        if World.logic_engine() is None:
            raise SMCException("Engine not set when creating world.")
        World.logic_engine().reset()
        self.addFluent(Fluent("time", "integer", []))
        self.__evaluationContext = LocalEvaluationContext(self, None)


    @property
    def evaluationContext(self):
        return self.__evaluationContext


    def getExpressionContext(self):
        return self.__expressionContext

    @staticmethod
    def create_new_world():
        """
        Creates a new instance for the singleton World and returns it.
        :rtype: World
        """
        World.__instance = World()
        return World.__instance

    def enumerate_fluent_instances(self, fluent):
        """
        Creates an iterator for instances of the given fluent
        :type fluent: Fluent
        """
        candidates = []
        candidate_indexes = []
        for p in fluent.parameters:
            dom = self.getDomain(p)
            # if the parameter's domain is empty, leave
            if len(dom) == 0:
                yield []
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
            World.logic_engine().setFluentValue(fluent.name, paramSelection, value)

    def check_fluent_initialization(self):
        """
        Checks whether all instances of all declared fluents and constants are initialized correctly.
        :return: tuple with (uninitialized_fluent_instances, uninitialized_constant_instances)
        :rtype: (list of (str, list), list of (str, list))
        """
        uninitialized_fluent_instances = []
        uninitialized_constant_instances = []
        for fluent in self.__fluents.values():
            """:type fluent: Fluent """
            for paramSelection in self.enumerate_fluent_instances(fluent):
                instance = None
                v = (self.getConstantValue(fluent.name, paramSelection) if isinstance(fluent, Constant)
                     else self.getFluentValue(fluent.name, paramSelection))
                if v is None:
                    instance = (fluent.name, paramSelection)
                    if isinstance(fluent, Constant):
                        uninitialized_constant_instances.append(instance)
                    else:
                        uninitialized_fluent_instances.append(instance)

        return uninitialized_fluent_instances, uninitialized_constant_instances

    def check_action_initialization(self):
        """
        Checks whether all stochastic and exogenous actions have been configured correctly.
        :return: tuple with (problematic_stochastic_actions, problematic_exogenous_actions) where each is a list of
        tuples with (action, promblem_list)

        :rtype: tuple
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

    def __create_expression_context(self):

        self.__expressionContext = dict()

        #: :type fluent: Fluent
        for fluent in self.__fluents.values():
            self.__expressionContext[fluent.name] = self.__make_fluent_access_function(fluent.name)

        def __fluentChangeClock(fluentName, *params):
            return self.getFluentChangeTime(fluentName, params)

        def __actionClock(actionName, *params):
            return self.getActionClock(actionName, params)

        self.__expressionContext['fluentClock'] = __fluentChangeClock
        self.__expressionContext['actionClock'] = __actionClock

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
            param_names = sa[1]
            outcome_names = sa[2]
            outcomes = []
            for o_name in outcome_names:
                outcomes.append(RandomActionOutcome(o_name))
            strategy = Deterministic() if len(outcomes) == 1 else Uniform()
            action = StochasticAction(sa[0], param_names, outcomes, strategy, immediate)
            self.addAction(action)

    def load_declarations(self):
        """
        Loads the declarations from the domain specification and initializes fluents, constants, and actions.
        """
        # fluentName -> core.Fluent
        self.__fluents = dict()
        # action_name -> core.Action
        self.__actions = dict()
        self.__exogenousActions = dict()
        declarations = World.logic_engine().load_declarations()
        for f in declarations['fluents']:
            self.addFluent(Fluent(f[0], f[2], f[1]))
        for c in declarations['constants']:
            self.addConstant(Constant(c[0], c[2], c[1]))
        for pa in declarations['primitive_actions']:
            immediate = pa[0] in declarations['immediate_actions']
            self.addAction(DeterministicAction(pa[0], pa[1], immediate))

        self.__load_stochastic_action_declarations(declarations["stochastic_actions"],
                                                   declarations["immediate_actions"])

        for ea in declarations['exogenous_actions']:
            self.addExogenousAction(ExogenousAction(ea[0], ea[1], ea[2]))

    def initialize(self, sample_fluent_values=True):
        """
        1. Sets up domains, i.e. defines the sets of entity objects for each sort.

        2. Optionally sets ups the a new initial situation by creating samples for the fluent instance for each combination
            of parameter values.
        """
        World.logic_engine().reset()
        self.__evaluationContext = LocalEvaluationContext(self, None)

        for sort in self.__domainMap.keys():
            oids = []
            for entity in self.__domainMap[sort]:
                oids.append(entity.id)
            World.logic_engine().defineDomain(sort, oids)

        #keep entityId->entity maps (__entities & __agents)
        self.__domainMap = dict()

        domain_map_from_eclipse = World.logic_engine().initSortHierarchy()

        for sort, domain in domain_map_from_eclipse.items():
            self.__domainMap[sort] = set()
            for entityId in domain:
                entity = None
                try:
                    entity = self.__entities[entityId]
                except KeyError:
                    raise SMCException("No Entity instance registered for {}:{}".format(entityId, sort))
                self.__domainMap[sort].add(entity)

        World.logic_engine().setFluentValue('time', [], 0)

        if sample_fluent_values:
            self.sample_fluent_values()

        self.__initialized = True
        self.__persistentPropertiesInitialized = False
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
        :rtype: list
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
        :raises: an SMCException if the given entity was not found in the registry.
        """
        if not entity.sortName in self.__domainMap:
            raise SMCException("Trying to remove entity {} with unregistered sort {}".format(
                entity.id, entity.sortName))
        eset = self.__domainMap[entity.sortName]
        if (not entity in eset) or (not entity.id in self.__entities):
            raise SMCException("Trying to remove unregistered entity {}.".format(entity.id))
        eset.remove(entity)
        self.__entities.pop(entity.id)
        if isinstance(entity, Agent):
            try:
                self.__agents.pop(entity.id)
            except KeyError:
                raise SMCException("Trying to remove unregistered agent {} from agent list.".format(entity.id))
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
        :raises SMCException if action is unregistered.
        :type action: Action
        """
        try:
            del self.__actions[action.name]
        except KeyError:
            raise SMCException("Trying to remove unregistered action %s ." % action.name)


    def getAction(self, action_name):
        """
        Returns the action (either deterministic or stochastic) with the given name.
        :raises SMCException if action is unregistered.
        :type action_name: str
        :rtype: Action
        """
        try:
            return self.__actions[action_name]
        except KeyError:
            raise (SMCException("Action {} not registered.".format(action_name)))

    def get_stochastic_action(self, action_name):
        """
        Returns the stochastic action with the given name.
        :type action_name: str
        :rtype: StochasticAction
        """
        a = self.getAction(action_name)
        if not isinstance(a, StochasticAction):
            raise SMCException("Action %s is not stochastic." % action_name)
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
            SMCException("Trying to erase unregistered exogenous action {}.".format(exogenousAction.action_name)))

    def get_exogenous_action(self, action_name):
        """
        Returns the exogenous action with the given name.
        :raises: SMCException if there is no exogenous action with the given name.
        :type action_name: str
        :rtype: ExogenousAction
        """
        try:
            return self.__exogenousActions[action_name]
        except KeyError:
            raise (SMCException("Unregistered exogenous action {}.".format(action_name)))

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
            raise (SMCException("Sort {} not declared.".format(sort_name)))

    def addFluent(self, fluent):
        """
        Adds a core.Fluent object to the metamodel.
        NOTE: this method should normally not be called directly but is automatically called in World.load_declarations().
        :type fluent: Fluent
        """
        self.__fluents[fluent.name] = fluent

    def addConstant(self, con):
        """
        Adds a constant to the metamodel.
        NOTE: this method should normally not be called directly but is automatically called in World.load_declarations().
        :type con: Constant
        """
        self.addFluent(con)

    def getFluents(self):
        """
        Returns a list view of all fluents currently registered in the metamodel as a list of core.Fluent objects.
        This list also included constants.
        :rtype: builtins.dict_values
        """
        return self.__fluents.values()

    def getConstants(self):
        """
        Returns a list of all registered constants.
        :rtype: list
        """
        return filter(lambda e: isinstance(e, Constant), self.__fluents.values())

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
        :raises: SMCException if a fluent has been registered with the given name that is not a constant.
        :type constantName: str
        :rtype: Constant
        """
        c = self.getFluent(constantName)
        if c is None:
            return None
        elif not isinstance(c, Constant):
            raise SMCException("Fluent {} is not a constant.".format(constantName))
        else:
            return c

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


    def __translate_action_execution(self, agent, action_execution):
        """
        Generates a ground deterministic action outcome from the given ActionExecution as a
        (action_name, [ground_params]) tuple. If actionExecution refers to a stochastic action,
        an outcome is generated according to the distribution that was defined for the action.
        The agent's evaluation context is used for generating an outcome.

        :return: (str, list)
        :raises: SMCException if action is not registered.

        :type agent: Agent
        :type action_execution: ActionExecution
        :rtype: (str, list)
        """
        try:
            action = self.__actions[action_execution.actionName]
        except KeyError:
            raise (
                SMCException("Trying to execute unregistered action: {}".format(
                    action_execution.actionName)))

        ground_params = agent.evaluation_context.resolve(*action_execution.actionParameters)

        if isinstance(action, StochasticAction):
            return action.generateOutcome(agent.evaluation_context, ground_params)
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

    def step(self):
        """
        Performs one discrete time step for all agents and runs the progression.

        :returns: self.__finished, overall_verdict, toplevel_results, scheduled_results, actions, failed_regular_actions
        :rtype: tuple
        """
        actions = []  # list of tuples: (action_execution, params)

        # gather actions
        new_step = True
        while True:
            self.__finished = True
            immediate_actions = []
            for agent in self.__agents.values():
                if not agent.is_finished():
                    self.__finished = False
                    action_execution = agent.step(new_step)
                    if action_execution is not None:
                        act = self.__translate_action_execution(agent, action_execution)

                        if act[0] in self.__actions and self.__actions[act[0]].immediate:
                            immediate_actions.append(act)
                        else:
                            actions.append(act)
                            agent.set_pending_action(act)
                            # progress immediate actions without succeeding time step
                            # leave loop only if we don't have any immediate actions left
            if len(immediate_actions) == 0:
                break
            new_step = False
            random.shuffle(immediate_actions)
            failed_actions = World.logic_engine().progress(immediate_actions)
            if moduleLogger.isEnabledFor(logging.DEBUG):
                moduleLogger.debug("Progressed immediately: %s", immediate_actions)
            if len(failed_actions) > 0:
                return self.__finished, NOT_OK, {}, {}, immediate_actions, failed_actions

        ex_act_instances = self.get_exogenous_action_instances()

        if ex_act_instances is not None:
            actions.extend(ex_act_instances)

        # regular action means non-exogenous actions
        failed_regular_actions = []

        if len(actions) > 0:
            # shuffle actions as means for achieving fairness
            random.shuffle(actions)
            failed_actions = World.logic_engine().progress(actions)
            if moduleLogger.isEnabledFor(logging.DEBUG):
                moduleLogger.debug("Progressed: %s", actions)

            failed_regular_actions = list(
                itertools.filterfalse(lambda fa: World.__get_action_name_from_term(fa) in self.__exogenousActions,
                                      failed_actions))

        if len(failed_regular_actions) > 0:
            return self.__finished, NOT_OK, {}, {}, actions, failed_regular_actions

        overall_verdict, toplevel_results, scheduled_results = World.logic_engine().evaluationStep()
        # it's or if events fail but if regular actions fail, we're in trouble!

        # TODO: distinguish between actions that take time and actions that don't
        # execute all non-time actions before taking a time step
        return self.__finished, overall_verdict, toplevel_results, scheduled_results, actions, []

    def runExperiment(self):
        """
        Runs the experiment that has been set up until a verdict can be determined or the world has finished.
        :return: (verdict, dict(steps, toplevel_results, scheduled_results, failedRegularActions))
        :rtype: tuple
        """
        step_num = 0
        verdict = NONDET
        toplevel_results = scheduled_results = failedRegularActions = []
        while (not self.is_finished()) and (verdict == NONDET):
            (_, verdict, toplevel_results, scheduled_results, _, failedRegularActions) = self.step()
            step_num += 1
        return (verdict,
                {'steps': step_num,
                 'toplevel_results': toplevel_results,
                 'scheduled_results': scheduled_results,
                 'failedActions': failedRegularActions
                })

    def runUntilFinished(self, maxSteps=None, maxRealTime=None, maxWorldTime=None, stepListeners=[]):
        """
        Repeatedly runs World.step() until either the world's finished flag becomes true or
        either the step or time limit is reached.

        :param maxSteps: int
        :param maxRealTime: datetime.timedelta
        """
        step_num = 0
        verdict = NONDET
        toplevel_results = scheduled_results = failedRegularActions = []
        c1 = c2 = time.clock()
        while not self.is_finished():
            (_, verdict, toplevel_results, scheduled_results, _, failedRegularActions) = self.step()

            c2 = time.clock()
            step_num += 1
            deltaT = c2 - c1
            for sl in stepListeners:
                sl(self, step_num, deltaT)

            if maxSteps != None and step_num >= maxSteps:
                break
            if maxRealTime != None and datetime.timedelta(seconds=deltaT) >= maxRealTime:
                break
            if maxWorldTime != None:
                t = self.getFluentValue('time', [])
                if t >= maxWorldTime:
                    break

        duration = datetime.timedelta(seconds=c2 - c1)
        worldTime = self.getFluentValue('time', [])
        return (verdict,
                {'steps': step_num,
                 'time': duration,
                 'worldTime': worldTime,
                 'toplevel_results': toplevel_results,
                 'scheduled_results': scheduled_results,
                 'failedActions': failedRegularActions
                }
        )

    def reset(self):
        World.logic_engine().reset(False, False) # don't remove formulas!
        self.__evaluationContext = LocalEvaluationContext(self, None)
        World.logic_engine().setFluentValue('time', [], 0)
        #: :type agent: Agent
        for agent in self.getAgents():
            agent.restart()

    def runRepetitions(self, numberOfRepetitions):
        # save state
        currentState = list(World.logic_engine().getCurrentState())
        results = []  # list of True/False


        for i in range(0, numberOfRepetitions):
            self.reset()
            World.logic_engine().restoreState(currentState)

            res = self.runExperiment()
            verdict = res[0] == constants.OK
            results.append(verdict)
            print("Experiment #{} --> {}".format(i + 1, verdict))

        self.reset()
        World.logic_engine().restoreState(currentState)

        return results

    def runSequentialHypothesisTest(self, p0, p1, alpha, beta):

        seqTest = SequentialAcceptanceTest(p0, p1, alpha, beta)

        currentState = list(World.logic_engine().getCurrentState())
        result = None
        m = 0
        verdicts = []
        numberOfDefects = 0
        while result is None:
            self.reset()
            World.logic_engine().restoreState(currentState)

            res = self.runExperiment()
            verdict = res[0] == constants.OK
            m += 1
            #if moduleLogger.isEnabledFor(logging.DEBUG):
            moduleLogger.debug("Sample #{} : {}".format(m, verdict))

            verdicts.append(verdict)
            if verdict == False:
                numberOfDefects += 1
            result = seqTest.checkAcceptance(m, numberOfDefects)

        self.reset()
        World.logic_engine().restoreState(currentState)

        return (result, m)

    def printState(self):
        if not self.__initialized: self.initialize()
        state = World.logic_engine().getCurrentState()
        for f in state:
            print(f)

    def registerProperty(self, propertyName, formula):
        '''
        Registers the given formula under the given name. 
        '''
        World.logic_engine().registerProperty(propertyName, formula)

    def getProperties(self):
        return World.logic_engine().getProperties()

    def getActionClock(self, actionName, params):
        '''
        Returns the last recorded time when the given action was galled with the given parameters.
        
        If the given action-parameter combination hasn't occurred before, this method returns -1.
        
        :param actionName: str
        :param params: list  
        
        :rtype: int
        '''
        return self.logic_engine().getActionClock(actionName, params)

    def getFluentChangeTime(self, fluentName, params):
        '''
        Returns the last recorded time when the given fluent with the given parameters.
        
        If the given fluent-parameter combination has not been initialized yet, 
        this method returns -1.
        
        :param fluentName: str
        :param params: list
        '''
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
    def __init__(self, contextEntity, parent):
        EvaluationContext.__init__(self, parent)
        self.__contextEntity = contextEntity
        self.__variableBindings = dict()  # variable name
        self.__variableBindings[Entity.SELF] = self.__contextEntity


    def evaluateCondition(self, sourceType, source, *params):
        resolvedParams = self.resolve(*params)
        result = None
        if sourceType == EvaluationContext.FLUENT:
            result = World.instance().getFluentValue(source, resolvedParams)
            if result == None:
                raise SMCException("No value found for fluent: {0}({1}).".format(source, resolvedParams))
        elif sourceType == EvaluationContext.ECLP_FUNCTION:
            result = World.logic_engine().evaluateCondition(source, *resolvedParams)
        elif sourceType == EvaluationContext.TRANSIENT_FLUENT:
            result = World.logic_engine().evaluateCondition(source, *resolvedParams, situation='s0')
        elif sourceType == EvaluationContext.CONSTANT:
            result = bool(World.getConstantValue(source, resolvedParams))
        elif sourceType == EvaluationContext.PYTHON_EXPRESSION:

            ctx = World.instance().getExpressionContext().copy()
            ctx.update(self.__variableBindings)
            ctx['params'] = params
            result = eval(source, ctx)
        else:
            result = source(*resolvedParams)

        return result

    def evaluateFunction(self, sourceType, source, *params):
        resolvedParams = self.resolve(*params)
        result = None
        if sourceType == EvaluationContext.FLUENT:
            fv = World.instance().getFluentValue(source, resolvedParams)
            if fv == None:
                raise SMCException("No value found for fluent: {0}({1}).".format(source, resolvedParams))
            result = fv
        elif sourceType == EvaluationContext.ECLP_FUNCTION:
            result = World.logic_engine().evaluateFunctionGoal(source, *resolvedParams)
        elif sourceType == EvaluationContext.TRANSIENT_FLUENT:
            result = World.logic_engine().evaluateFunctionGoal(source, *resolvedParams, situation='s0')
        elif sourceType == EvaluationContext.CONSTANT:
            result = World.instance.getConstantValue(source, resolvedParams)
        elif sourceType == EvaluationContext.PYTHON_EXPRESSION:
            ctx = World.instance().getExpressionContext().copy()
            ctx.update(self.__variableBindings)
            ctx['params'] = params
            result = eval(source, ctx)
        else:
            result = source(*resolvedParams)

        if isinstance(result, str):
            result = self.getEntity(result)
        return result

    def getFluentValue(self, fluentName, *params):
        resolvedParams = self.resolve(*params)
        fv = World.instance().getFluentValue(fluentName, resolvedParams)
        if fv == None:
            raise SMCException("No value found for fluent: {0}({1}).".format(fluentName, resolvedParams))
        return fv


    def assignVariable(self, variableName, value):
        '''
        variableName: procedure.Variable that should be set. 
        functionName: name of the goal that is used for evaluation
        functionParams: parameters for goal evaluation excluding the implicit last parameter that is used 
                    for the result, i.e. the evaluated goals must have a signature like P(X1,X2,...,Xn-1,R):-... where
                    R is used for the result.
        '''

        self.__variableBindings[variableName] = value


    def resolve(self, *terms):
        '''
        Evaluates each term in terms and returns a list with the collected results.
        
        Entity instances are converted to their ids.
        '''
        groundTerms = []
        for term in terms:
            gt = term
            if term is Entity.SELF:
                gt = self.__contextEntity.id
            elif isinstance(term, Variable):
                if not term.name in self.__variableBindings:
                    raise SMCException("Variable %s not bound." % term.name)
                gt = self.__variableBindings[term.name]

            if isinstance(gt, Entity):
                gt = gt.id

            groundTerms.append(gt)

        return groundTerms

    def getEntity(self, entityId):
        '''
        returns the entity with the given id
        '''
        return World.instance().getEntityById(entityId)


    def selectAll(self, predicateType, predicateName, *params):
        '''
        Returns a list of dicts with {paramName => Entity} entries that fulfill the given predicate with 
        the given parameters. 
        
        The parameter list can include ground values, bound variables and (name, sort) tuples.
        '''
        resolvedParams = self.resolve(*params) # the free variables tuples are ignored by resolve()

        sit = 's0' if predicateType in [EvaluationContext.FLUENT, EvaluationContext.TRANSIENT_FLUENT] else None

        resultList = World.logic_engine().selectAll(predicateName, *resolvedParams, situation=sit)
        refinedResult = []

        #: :type valueCombination: dict 
        for valueCombination in resultList:
            refinedEntry = dict()
            for name, value in valueCombination.items():
                #TODO: handle params with interval domains?
                if isinstance(value, str):
                    refinedEntry[name] = self.getEntity(value)
                else:
                    refinedEntry[name] = value

            refinedResult.append(refinedEntry)

        return refinedResult

    def selectFirst(self, predicateType, predicateName, *params):
        '''
        Returns a dict with {paramName => Entity} entries that represents the first value combination
        that fulfills the given predicate with the given parameters. 
        
        The parameter list can include ground values, bound variables and (name, sort) tuples.
        '''
        resolvedParams = self.resolve(*params) # the free variables tuples are ignored by resolve()

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
            params = action[1:] # remember: this slicing will return  [] if there's no more than 1 elements in action
            refinedPlan.append(ActionExecution(actionName, params))

        return refinedPlan, refinedValues

    def createChildContext(self):
        return LocalEvaluationContext(self.__contextEntity, self)

    def getSorts(self):
        return World.instance().getSorts()

    def getDomain(self, sortName):
        return World.instance().getDomain(sortName)
    