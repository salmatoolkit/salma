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
from salma.model.core import Entity, Agent, Constant, DeterministicAction, ExogenousAction
from salma.model.evaluationcontext import EvaluationContext
from salma.model.procedure import ActionExecution

from ..statistics import SequentialAcceptanceTest
from .core import Agent, Entity, Fluent, StochasticAction
from .procedure import Variable, ActionExecution


MODULE_LOGGER_NAME = 'agamemnon-smc.world'
moduleLogger = logging.getLogger(MODULE_LOGGER_NAME)


class World(Entity):
    '''
    classdocs
    '''

    # singleton reference
    # : :type logicsEngine: Engine
    logicsEngine = None

    SORT_NAME = 'world'
    ENTITY_ID = 'world'

    def getLogicsEngine(self):
        return World.logicsEngine

    __instance = None


    def __init__(self):
        Entity.__init__(self, World.ENTITY_ID, World.SORT_NAME)

        # fluentName -> core.Fluent
        self.__fluents = dict()
        # actionName -> core.Action
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

        if World.logicsEngine is None:
            raise SMCException("Engine not set when creating world.")
        World.logicsEngine.reset()
        self.addFluent(Fluent("time", "integer", [], range=(0, sys.maxsize)))
        self.__evaluationContext = LocalEvaluationContext(self, None)


    @property
    def evaluationContext(self):
        return self.__evaluationContext


    def getExpressionContext(self):
        return self.__expressionContext

    @staticmethod
    def createNewWorld():
        World.__instance = World()
        return World.__instance


    def enumerateFluentInstances(self, fluent):
        '''
        :type fluent: Fluent
        '''
        candidates = []
        candidateIndexes = []
        for p in fluent.parameters:
            dom = self.getDomain(p)
            # if the parameter's domain is empty, leave
            if len(dom) == 0:
                yield []
                return
            candidates.append(list(dom))
            candidateIndexes.append(0)

            # todo: handle fluents without parameters
        finished = False
        while not finished:
            paramSelection = []
            for i, c in enumerate(candidates):
                paramSelection.append(c[candidateIndexes[i]].id)
            yield paramSelection

            # increment first position similar to binary counter
            if len(candidateIndexes) > 0:
                candidateIndexes[0] += 1
                # propagate increment
                for i, c in enumerate(candidates):
                    if candidateIndexes[i] > len(c) - 1:
                        # stop at the last candidate list = parameter position
                        if i == len(candidates) - 1:
                            finished = True
                        else:
                            candidateIndexes[i] = 0
                            candidateIndexes[i + 1] += 1
            else:
                finished = True


    def __initializeFluent(self, fluent):
        '''
        Creates samples for the fluent instance for each combination
            of parameter values.
        '''
        # list of domain lists
        for paramSelection in self.enumerateFluentInstances(fluent):
            value = fluent.generateSample(self.__evaluationContext, paramSelection)
            World.logicsEngine.setFluentValue(fluent.name, paramSelection, value)


    def checkFluentInitialization(self):
        uninitialized_fluent_instances = []
        uninitialized_constant_instances = []
        for fluent in self.__fluents.values():
            """:type fluent: Fluent """
            for paramSelection in self.enumerateFluentInstances(fluent):
                instance = None
                v = (self.getConstantValue(fluent.name, paramSelection) if isinstance(fluent, Constant)
                     else self.getFluentValue(fluent.name, paramSelection))
                if v is None:
                    instance = (fluent.name, paramSelection)
                if isinstance(fluent, Constant):
                    uninitialized_constant_instances.append(instance)
                else:
                    uninitialized_fluent_instances.append(instance)

        return (uninitialized_fluent_instances, uninitialized_constant_instances)


    def __makeFluentAccessFunction(self, fluentName):
        def __f(*params):
            return self.getFluentValue(fluentName, params)

        return __f


    def __createExpressionContext(self):

        self.__expressionContext = dict()

        #: :type fluent: Fluent
        for fluent in self.__fluents.values():
            self.__expressionContext[fluent.name] = self.__makeFluentAccessFunction(fluent.name)

        def __fluentChangeClock(fluentName, *params):
            return self.getFluentChangeTime(fluentName, params)

        def __actionClock(actionName, *params):
            return self.getActionClock(actionName, params)

        self.__expressionContext['fluentClock'] = __fluentChangeClock
        self.__expressionContext['actionClock'] = __actionClock


    def sampleFluentValues(self):
        for fluent in itertools.filterfalse(lambda f: f.name == 'time', self.__fluents.values()):
            self.__initializeFluent(fluent)

    def load_declarations(self):
        '''
        Loads the declarations from the domain specification and initializes fluents, constants, and actions.
        '''
        # fluentName -> core.Fluent
        self.__fluents = dict()
        # actionName -> core.Action
        self.__actions = dict()
        self.__exogenousActions = dict()
        declarations = World.logicsEngine.load_declarations()
        for f in declarations['fluents']:
            self.addFluent(Fluent(f[0], f[2], f[1]))
        for c in declarations['constants']:
            self.addConstant(Constant(c[0],c[2],c[1]))
        for pa in declarations['primitive_actions']:
            immediate = pa[0] in declarations['immediate_actions']
            self.addAction(DeterministicAction(pa[0],pa[1]), immediate)
        for sa in declarations['stochastic_actions']:
            immediate = sa[0] in declarations['immediate_actions']
            self.addAction(DeterministicAction(sa[0],sa[1]), immediate)
        for ea in declarations['exogenous_actions']:
            self.addExogenousAction(ExogenousAction(ea[0], ea[1], ea[2]))

    def initialize(self, sampleFluentValues=True):
        '''
        1. Sets up domains, i.e. defines the sets of entity objects for each sort.
        
        2. Sets ups the a new initial situation by creating samples for the fluent instance for each combination
            of parameter values.
        '''
        World.logicsEngine.reset()
        self.__evaluationContext = LocalEvaluationContext(self, None)

        for sort in self.__domainMap.keys():
            oids = []
            for entity in self.__domainMap[sort]:
                oids.append(entity.id)
            World.logicsEngine.defineDomain(sort, oids)

        #keep entityId->entity maps (__entities & __agents)
        self.__domainMap = dict()

        domainMapFromEclipse = World.logicsEngine.initSortHierarchy()

        for sort, domain in domainMapFromEclipse.items():

            self.__domainMap[sort] = set()

            for entityId in domain:
                entity = None
                try:
                    entity = self.__entities[entityId]
                except KeyError:
                    raise SMCException("No Entity instance registered for {}:{}".format(entityId, sort))

                self.__domainMap[sort].add(entity)

        World.logicsEngine.setFluentValue('time', [], 0)

        if sampleFluentValues:
            self.sampleFluentValues()

        self.__initialized = True
        self.__persistentPropertiesInitialized = False
        self.__createExpressionContext()

    def isFinished(self):
        '''
        Returns true if every registered agent has finished already.
        '''

        return self.__finished


    def getAllEntities(self):
        return self.__entities.values()

    def getAgents(self, sort=None):
        '''
        Returns a list of all agents, optionally restricted to the given sort.
        '''
        entities = self.getAllEntities() if sort is None else self.getDomain(sort)
        result = []
        for e in entities:
            if isinstance(e, Agent):
                result.append(e)
        return result


    def getEntityById(self, entityId):
        return self.__entities[entityId]


    def addEntity(self, entity):
        self.__entities[entity.id] = entity
        if isinstance(entity, Agent):
            self.__agents[entity.id] = entity
        if entity.sortName in self.__domainMap:
            self.__domainMap[entity.sortName].add(entity)
        else:
            self.__domainMap[entity.sortName] = set([entity])


    def addAgent(self, agent):
        self.addEntity(agent)
        agent.setEvaluationContext(LocalEvaluationContext(agent, None))


    def removeEntity(self, entity):

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
        self.removeEntity(agent)
        agent.evaluationContext = None

    def addAction(self, action):
        self.__actions[action.name] = action

    def removeAction(self, action):
        if action.name in self.__actions:
            del self.__actions[action.name]

    def getAction(self, action_name):
        try:
            return self.__actions[action.name]
        except KeyError:
            raise(SMCException("Action {} not registered.".format(action_name)))

    def setOutcomeSelector(self, action_name, selector):
        '''
        Sets the outcome selector function for the stochastic action with the given name.
        :param action_name:
        :param selector:

        :type action_name: str
        '''
        action = self.getAction(action_name)
        if not isinstance(action, StochasticAction):
            raise(SMCException("Action {} is not a stochastic action.".format(action_name)))
        action.setOutcomeSelector(selector)

    def getAllActions(self):
        return self.__actions.values()


    def addExogenousAction(self, exogenousAction):
        ':type exogenousAction: ExogenousAction'
        self.__exogenousActions[exogenousAction.actionName] = exogenousAction

    def removeExogenousAction(self, exogenousAction):
        ':type exogenousAction: ExogenousAction'
        if exogenousAction.actionName in self.__exogenousActions:
            del self.__exogenousActions[exogenousAction.actionName]

    def getExogenousAction(self, action_name):
        '''
        :rtype: salma.model.core.ExogenousAction
        '''

        try:
            return self.__exogenousActions[action_name]
        except KeyError:
            raise(SMCException("Unregistered exogenous action {}.".format(action_name)))

    def getAllExogenousActions(self):
        return self.__exogenousActions.values()

    def configureExogenousAction(self, action_name, occurance_distribution, qualifying_parameter_distributions):
        '''
        :type action_name: str
        :type occurance_distribution: salma.model.distributions.Distribution
        :type qualifying_parameter_distributions: list
        '''
        act = self.getExogenousAction(action_name)
        act.set_occurance_distribution(occurance_distribution)
        act.set_qualifying_param_distributions(qualifying_parameter_distributions)

    def getSorts(self):
        return self.__domainMap.keys()

    def getDomain(self, sortName):
        if sortName in self.__domainMap:
            return self.__domainMap[sortName]
        else:
            raise (SMCException("Sort {} not declared.".format(sortName)))

    def addFluent(self, fluent):
        '''
        Adds a core.Fluent object to the metamodel.
        
        fluent: core.Fluent object

        '''
        self.__fluents[fluent.name] = fluent


    def addConstant(self, con):
        self.addFluent(con)

    def getFluents(self):
        '''
        Returns all fluents currently registerd in the metamodel as a list of core.Fluent objects.
        return
        '''
        return self.__fluents.values()

    def getConstants(self):
        return filter(lambda e: isinstance(e, Constant), self.__fluents.values())

    def getFluent(self, fluentName):
        '''
        Returns the core.Fluent object associated by the given fluent name or None if such a fluent
        hasn't been registered.
        '''
        if fluentName in self.__fluents:
            return self.__fluents[fluentName]
        else:
            return None

    def getConstant(self, constantName):
        c = self.getFluent(constantName)
        if (c is None) or (not isinstance(c, Constant)):
            return None
        else:
            return c


    @staticmethod
    def getInstance():
        '''
        :rtype: World
        '''
        if World.__instance is None:
            World.__instance = World()
        return World.__instance


    def getFluentValue(self, fluentName, fluentParams):
        '''
        :param fluentName: str
        :param fluentParams: parameters
        '''
        if not self.__initialized: self.initialize()
        # we don't check if the fluent has been registered for performance reasons
        fv = World.logicsEngine.getFluentValue(fluentName, *fluentParams)
        if fv == None:
            return None
        else:
            return fv.value

    def getTime(self):
        return self.getFluentValue('time', [])


    def getConstantValue(self, constantName, constantParams):
        if not self.__initialized: self.initialize()
        return World.logicsEngine.getConstantValue(constantName, constantParams)


    def setFluentValue(self, fluentName, fluentParams, value):
        if not self.__initialized: self.initialize()
        World.logicsEngine.setFluentValue(fluentName, fluentParams, value)

    def setConstantValue(self, constantName, constantParams, value):
        if not self.__initialized: self.initialize()
        World.logicsEngine.setConstantValue(constantName, constantParams, value)


    def __translateActionExecution(self, agent, actionExecution):
        '''
        Generates a ground deterministic action outcome from the given ActionExecution as a 
        (actionName, [groundParams]) tuple. If actionExecution refers to a stochastic action,
        an outcome is generated according to the distribution that was defined for the action.
        The agent's evaluation context is used for generating an outcome.
        
        :type agent: Agent
        :type actionExecution: ActionExecution
        :rtype (str, list) 
        '''

        try:
            action = self.__actions[actionExecution.actionName]
        except KeyError:
            raise (
                SMCException("Trying to execute unregistered action: {}".format(
                    actionExecution.actionName)))

        groundParams = agent.evaluationContext.resolve(*actionExecution.actionParameters)

        if isinstance(action, StochasticAction):
            return action.generateOutcome(agent.evaluationContext, groundParams)
        else:
            return (actionExecution.actionName, groundParams)

    @staticmethod
    def __getActionNameFromTerm(actionTerm):
        '''
        :param actionTerm: (pyclp.Atom, pyclp.Compound)
        :rtype str
        '''
        if isinstance(actionTerm, pyclp.Compound):
            return actionTerm.functor()
        else:
            return str(actionTerm)


    def getExogenousActionInstances(self):
        candidates = World.logicsEngine.getExogenousActionCandidates()
        eaInstances = []
        for candidate in candidates.items():
            actionName = candidate[0]
            combinations = candidate[1]
            # just ignore if the ex action is not registered
            if actionName in self.__exogenousActions:
                ea = self.__exogenousActions[actionName]
                for params in combinations:
                    if ea.shouldHappen(self.__evaluationContext, params):
                        instance = ea.generateInstance(self.__evaluationContext, params)
                        eaInstances.append(instance)
        return eaInstances


    def step(self):
        '''
        Performs one discrete time step for all agents and runs the progression.
        '''
        if not self.__initialized: self.initialize()

        actions = []  # list of tuples: (actionExecution, params)

        # gather actions


        newStep = True
        while True:
            self.__finished = True
            immediateActions = []
            for agent in self.__agents.values():
                if not agent.isFinished():
                    self.__finished = False
                    actionExecution = agent.step(newStep)
                    if actionExecution != None:
                        act = self.__translateActionExecution(agent, actionExecution)

                        if act[0] in self.__actions and self.__actions[act[0]].immediate:
                            immediateActions.append(act)
                        else:
                            actions.append(act)
                            agent.setPendingAction(act)

                            # progress immediate actions without succeeding time step
                            # leave loop only if we don't have any immediate actions left
            if len(immediateActions) == 0:
                break
            newStep = False
            random.shuffle(immediateActions)
            failedActions = World.logicsEngine.progress(immediateActions)
            if moduleLogger.isEnabledFor(logging.DEBUG):
                moduleLogger.debug("Progressed immediately: %s", immediateActions)
            if len(failedActions) > 0:
                return (self.__finished, NOT_OK, {}, {}, immediateActions, failedActions)

                # TODO: add events
                # : :type exAct: ExogenousAction

        exActInstances = self.getExogenousActionInstances()

        if exActInstances != None:
            actions.extend(exActInstances)


        # shuffle actions as means for achieving fairness
        failedRegularActions = []

        if len(actions) > 0:
            random.shuffle(actions)
            failedActions = World.logicsEngine.progress(actions)
            if moduleLogger.isEnabledFor(logging.DEBUG):
                moduleLogger.debug("Progressed: %s", actions)

            failedRegularActions = list(
                itertools.filterfalse(lambda fa: World.__getActionNameFromTerm(fa) in self.__exogenousActions,
                                      failedActions))

        if len(failedRegularActions) > 0:
            return (self.__finished, NOT_OK, {}, {}, actions, failedRegularActions)

        overallVerdict, toplevel_results, scheduled_results = World.logicsEngine.evaluationStep()
        # it's or if events fail but if regular actions fail, we're in trouble! 



        # TODO: distinguish between actions that take time and actions that don't
        # execute all non-time actions before taking a time step


        return (self.__finished, overallVerdict, toplevel_results, scheduled_results, actions, [])


    def runExperiment(self):
        step_num = 0
        verdict = NONDET
        toplevel_results = scheduled_results = failedRegularActions = []
        while (not self.isFinished()) and (verdict == NONDET):
            (_, verdict, toplevel_results, scheduled_results, _, failedRegularActions) = self.step()
            step_num += 1
        return (verdict,
                {'steps': step_num,
                 'toplevel_results': toplevel_results,
                 'scheduled_results': scheduled_results,
                 'failedActions': failedRegularActions
                }
        )


    def runUntilFinished(self, maxSteps=None, maxRealTime=None, maxWorldTime=None, stepListeners=[]):
        '''
        Repeatedly runs World.step() until either the world's finished flag becomes true or
        either the step or time limit is reached.
        
        :param maxSteps: int   
        :param maxRealTime: datetime.timedelta
        '''
        step_num = 0
        verdict = NONDET
        toplevel_results = scheduled_results = failedRegularActions = []
        c1 = c2 = time.clock()
        while not self.isFinished():
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
        World.logicsEngine.reset(False, False) # don't remove formulas!
        self.__evaluationContext = LocalEvaluationContext(self, None)
        World.logicsEngine.setFluentValue('time', [], 0)
        #: :type agent: Agent
        for agent in self.getAgents():
            agent.restart()


    def runRepetitions(self, numberOfRepetitions):
        # save state
        currentState = list(World.logicsEngine.getCurrentState())
        results = [] # list of True/False

        # TODO: also reset agents

        for i in range(0, numberOfRepetitions):
            self.reset()
            World.logicsEngine.restoreState(currentState)

            res = self.runExperiment()
            verdict = res[0] == constants.OK
            results.append(verdict)
            print("Experiment #{} --> {}".format(i + 1, verdict))

        self.reset()
        World.logicsEngine.restoreState(currentState)

        return results


    def runSequentialHypothesisTest(self, p0, p1, alpha, beta):

        seqTest = SequentialAcceptanceTest(p0, p1, alpha, beta)

        currentState = list(World.logicsEngine.getCurrentState())
        result = None
        m = 0
        verdicts = []
        numberOfDefects = 0
        while result is None:
            self.reset()
            World.logicsEngine.restoreState(currentState)

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
        World.logicsEngine.restoreState(currentState)

        return (result, m)


    def printState(self):
        if not self.__initialized: self.initialize()
        state = World.logicsEngine.getCurrentState()
        for f in state:
            print(f)


    def registerProperty(self, propertyName, formula):
        '''
        Registers the given formula under the given name. 
        '''
        World.logicsEngine.registerProperty(propertyName, formula)

    def getProperties(self):
        return World.logicsEngine.getProperties()


    def getActionClock(self, actionName, params):
        '''
        Returns the last recorded time when the given action was galled with the given parameters.
        
        If the given action-parameter combination hasn't occurred before, this method returns -1.
        
        :param actionName: str
        :param params: list  
        
        :rtype: int
        '''
        return self.logicsEngine.getActionClock(actionName, params)

    def getFluentChangeTime(self, fluentName, params):
        '''
        Returns the last recorded time when the given fluent with the given parameters.
        
        If the given fluent-parameter combination has not been initialized yet, 
        this method returns -1.
        
        :param fluentName: str
        :param params: list
        '''
        return self.logicsEngine.getFluentChangeTime(fluentName, params)

    def queryPersistentProperty(self, propertyName):
        '''
        Returns a tuple with the current status of the given property together with the last
        change time.
        
        :param propertyName: str
        '''
        return self.logicsEngine.queryPersistentProperty(propertyName)


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
            result = World.getInstance().getFluentValue(source, resolvedParams)
            if result == None:
                raise SMCException("No value found for fluent: {0}({1}).".format(source, resolvedParams))
        elif sourceType == EvaluationContext.ECLP_FUNCTION:
            result = World.logicsEngine.evaluateCondition(source, *resolvedParams)
        elif sourceType == EvaluationContext.TRANSIENT_FLUENT:
            result = World.logicsEngine.evaluateCondition(source, *resolvedParams, situation='s0')
        elif sourceType == EvaluationContext.CONSTANT:
            result = bool(World.getConstantValue(source, resolvedParams))
        elif sourceType == EvaluationContext.PYTHON_EXPRESSION:

            ctx = World.getInstance().getExpressionContext().copy()
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
            fv = World.getInstance().getFluentValue(source, resolvedParams)
            if fv == None:
                raise SMCException("No value found for fluent: {0}({1}).".format(source, resolvedParams))
            result = fv
        elif sourceType == EvaluationContext.ECLP_FUNCTION:
            result = World.logicsEngine.evaluateFunctionGoal(source, *resolvedParams)
        elif sourceType == EvaluationContext.TRANSIENT_FLUENT:
            result = World.logicsEngine.evaluateFunctionGoal(source, *resolvedParams, situation='s0')
        elif sourceType == EvaluationContext.CONSTANT:
            result = World.getInstance.getConstantValue(source, resolvedParams)
        elif sourceType == EvaluationContext.PYTHON_EXPRESSION:
            ctx = World.getInstance().getExpressionContext().copy()
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
        fv = World.getInstance().getFluentValue(fluentName, resolvedParams)
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
        return World.getInstance().getEntityById(entityId)


    def selectAll(self, predicateType, predicateName, *params):
        '''
        Returns a list of dicts with {paramName => Entity} entries that fulfill the given predicate with 
        the given parameters. 
        
        The parameter list can include ground values, bound variables and (name, sort) tuples.
        '''
        resolvedParams = self.resolve(*params) # the free variables tuples are ignored by resolve()

        sit = 's0' if predicateType in [EvaluationContext.FLUENT, EvaluationContext.TRANSIENT_FLUENT] else None

        resultList = World.logicsEngine.selectAll(predicateName, *resolvedParams, situation=sit)
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

        result = World.logicsEngine.selectFirst(predicateName, *resolvedParams, situation=sit)
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
        plan, values = World.logicsEngine.createPlan(procedureName,
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
        return World.getInstance().getSorts()

    def getDomain(self, sortName):
        return World.getInstance().getDomain(sortName)
    