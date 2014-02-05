'''
Created on 21.05.2013

@author: kroiss
'''

import logging
import numbers

import pyclp
from salma.SALMAException import SALMAException
from .constants import *
import salma
import os

MODULE_LOGGER_NAME = 'agamemnon-smc.egine'
moduleLogger = logging.getLogger(MODULE_LOGGER_NAME)



class FluentValue(object):
    '''
    Stores the string representation of a fluent value together with its associated parameters.
    Note that no type is stored here.
    '''    
    def __init__(self, fluentName, fluentParamValues, value):
        '''
        fluentName: fluent name
        fluentParamValues: list of params
        value: fluent value in string form
        '''
        self.__fluentName = fluentName
        self.__fluentParamValues = fluentParamValues
        self.__value = value

    def getFluentName(self): return self.__fluentName
    fluentName = property(getFluentName)
    
    def getFluentParamValues(self): return self.__fluentParamValues
    fluentParamValues = property(getFluentParamValues)
    
    def getValue(self): 
        return self.__value
    value = property(getValue)
    
    
    def __str__(self):
        s = self.__fluentName 
        if len(self.__fluentParamValues) > 0: 
            s += "("
            for p in self.__fluentParamValues[:-1]:
                s += p + ", "
            s += str(self.__fluentParamValues[-1]) + ")"
            
        s += " = " + str(self.__value) 
        return s
    

class Engine(object):
    """
    Abstract
    """
    def getCurrentState(self):
        '''
        returns a list of FluentValue objects.
        '''
        raise NotImplementedError()
    
    def restoreState(self, fluentValues):
        raise NotImplementedError()
    
    def getFluentValue(self, fluentName, *fluentParams):
        '''
        returns: the FluentValue object
        '''
        raise NotImplementedError()
    
    def setFluentValue(self, fluentName, fluentParams, value):
        raise NotImplementedError()
    def cleanup(self):
        '''
        Resets the values of all fluents.
        '''
        raise NotImplementedError()
    
    def progress(self, actions):
        '''
        Performs a progression of all fluents with the given actions.
        '''
        raise NotImplementedError()
    
    def evaluateCondition(self, conditionGoalName, *conditionGoalParams):
        '''
        Evaluates the given goal and returns true if the evaluation succeeds.

        conditionGoalName: name of the condition goal. The goal has to be defined in the domain module.
        conditionGoalParams: the parameters that are used for evaluating the condition goal.
        returns: true if evaluation succeeded
        '''
        raise NotImplementedError()
   
    def evaluateFunctionGoal(self, situation, goalName, *goalParams):
        '''
        Evaluates the given goal with the given parameters and an implicit last parameter for the result variable.
        Only the first result is returned.
        
        If a situation is given, it is appended after the result variable at the last position. 
        ''' 
        raise NotImplementedError()
    
    def evaluateRelationalGoal(self, situation, goalName, *goalParams):
        '''
        Evaluates the given goal with the given parameters and an implicit last parameter for the result variable.
        A list of all found results is returned.
        
        If a situation is given, it is appended after the result variable at the last position. 
        ''' 
        raise NotImplementedError()
    
    def selectAll(self, predicateName, *params, **kwargs):
        '''
        returns a list of tuples with all possible value combinations for the given variables that make the execution of the given predicate 
        succeed
        
        :param predicateName str:
        :param params list: list of either concrete values or (name, type) tuples  
        :rtype list: list of dicts
        '''
        raise NotImplementedError()
    
    def selectFirst(self, predicateName, *params, **kwargs):
        '''
        returns a tuple with the first possible value combination that make the execution of the given predicate 
        succeed
        
        :param params list: list of either concrete values or (name, type) tuples  
        :param predicateName str: 
        :rtype dict: 
        '''
        raise NotImplementedError()       
    
    def createPlan(self, procedureName, *params, **kwargs):
        '''
        Creates a plan for the given procedure.
        
        returns a tuple: (plan = list of ground ActionExecutions, dict with params)
        '''
        raise NotImplementedError()
    
    def reset(self):
        raise NotImplementedError()
    
    def defineDomain(self, sortName, objectIds):
        '''
        Defines the domain of the given sort to be the given list of objectIds
        
        :param sortName: str
        :param objectIds: list  
        '''
        raise NotImplementedError()
   
    def initSortHierarchy(self):
        '''
        Resolves all declared sort - subsort relationships and initializes supersorts
        accordingly.
        
        '''
        raise NotImplementedError()
        
    def setConstantValue(self, constantName, constantParams, value):
        '''
        sets the given constant value for the given params
        
        The value is cached and all subsequent calls to getConstantValue() will return
        the cached value.
        
        :param constantName: str
        :param constantParams: list
        :param value: object    
        '''
        raise NotImplementedError()
    
    def getConstantValue(self, constantName, constantParams):
        '''
        Returns the cached value for the given constant with the given parameters. If no
        value is set, it returns None.
      
        :param constantName: str
        :param constantParams: list
        
        '''
        raise NotImplementedError()
    
    def isConstantDefined(self, constantName, constantParams):
        '''
        Returns true iff a value is defined for the given constant with the given parameters.
        
        :param constantName: str
        :param constantParams: list
        '''
        raise NotImplementedError()
        
        
    def evaluationStep(self):
        '''
        returns a tuple with a verdict and two dicts: (verdict, toplevel_results, scheduled_results) 
        toplevel_results: propertyName : verdict (OK, NOT_OK, NONDET)
        scheduled_results: (propertyName, time) : verdict
        :rtype (verdict, dict)
        '''
        raise NotImplementedError()
    
    def registerProperty(self, propertyName, formula):
        '''
        Registers the given formula under the given name. 
        '''
        raise NotImplementedError()
    def printToplevelGoals(self):
        raise NotImplementedError()
    
    def getProperties(self):
        '''
        Returns a dict with name : formula
        '''
        raise NotImplementedError()
        
    def getExogenousActionCandidates(self):
        '''
        Returns a list of form [action_name : [ [x1_1, x2_1, ...], [x1_2, x2_2, ...], ...], actionName2 : ... ]
        '''
        raise NotImplementedError()
    
    def getActionClock(self, actionName, params):
        '''
        Returns the last recorded time when the given action was galled with the given parameters.
        
        If the given action-parameter combination hasn't occurred before, this method returns -1.
        
        :param actionName: str
        :param params: list  
        
        :rtype: int
        '''
        raise NotImplementedError()
    
    def getFluentChangeTime(self, fluentName, params):
        '''
        Returns the last recorded time when the given fluent with the given parameters.
        
        If the given fluent-parameter combination has not been initialized yet, 
        this method returns -1.
        
        :param fluentName: str
        :param params: list
        '''
        raise NotImplementedError()
    
    def queryPersistentProperty(self, propertyName):
        '''
        Returns a tuple with the current status of the given property together with the last
        change time.
        
        :param propertyName: str
        :rtype: tuple
        '''
        raise NotImplementedError()
    
    def load_declarations(self):
        """
        Compiles all declarations of fluents, constants and actions from the ECLiPSe model. Returns a
        dict with the category as key and a list of tuples as value. The keys are:
        fluents, constants, primitive_actions, stochastic_actions, exogenous_actions.

        :rtype: dict
        """
        raise NotImplementedError()
    
def createParamTerms(*params, **kwargs):
    """
    Converts the parameters in params into proper values for PyCLP goal calls.
    * 0 --> zero since strange PyCLP behavior
    * True --> true
    * -nested tuples with len > 1: ('op', x, y, ...) --> pyclp.Compound with 'op' as functor
    and x,y,... converted by recursive call to createParamTerms.

    If kwargs contains a 'situation' argument, then this is appended at the last position.

    :rtype: list of pyclp.Term
    """
    paramTerms = []
    for p in params:
        term = None
        if isinstance(p, bool):
            term = pyclp.Atom('true') if p == True else pyclp.Atom('false')
        elif isinstance(p, numbers.Number):
            if p == 0:
                term = pyclp.Atom('zero')
            else:
                term = p
        elif isinstance(p, pyclp.Var):
            term = p
        elif isinstance(p, tuple) and len(p) > 0:
            term = pyclp.Compound(str(p[0]), *createParamTerms(*p[1:]))
        elif isinstance(p, list):
            subterms = []
            for subterm in p:
                subterms.append(createParamTerms(subterm)[0])
            term = pyclp.PList(subterms)
        else:
            term = pyclp.Atom(str(p))
        paramTerms.append(term)
    situation = kwargs['situation'] if 'situation' in kwargs else None
    if situation != None:
        paramTerms.append(pyclp.Atom(situation))
        
    return paramTerms
 
def createTerm(functor, *params, **kwargs):
    paramTerms = createParamTerms(*params, **kwargs)
    
    if len(paramTerms) == 0:
        return pyclp.Atom(functor)
    else:
        return pyclp.Compound(functor, *paramTerms) # must unpack because otherwise the term would contain brackets 
            

class EclipseCLPEngine(Engine):
    '''
    classdocs
    '''
    #TODO: make this somehow more module relative
    PROGRESSION_MODULE = os.path.abspath(
        os.path.join(salma.__path__[0], "../ecl-src/agasmc.ecl"))


    __verdictMapping = {'ok' : OK, 'not_ok' : NOT_OK, 'nondet' : NONDET}

    def __init__(self, domainPath, procedureDefPath = None):
        # make absolute path

        self.__domainPath = os.path.abspath(domainPath)

        self.__proceduresPath = None if procedureDefPath is None else os.path.abspath(procedureDefPath)
        self.__properties = dict()
        self.__constants = dict()
        
        
        # dict: (fluentName, tuple(fluentParams)) -> engine.FluentValue
        self.__currentState = None
        
        pyclp.init()  # Init ECLiPSe engine
        lib_path_var = pyclp.Var()
        domain_path_var = pyclp.Var()
        procedures_def_path_var = pyclp.Var()
        pyclp.Compound("os_file_name", lib_path_var, EclipseCLPEngine.PROGRESSION_MODULE).post_goal()
        pyclp.Compound("compile", lib_path_var).post_goal()
        pyclp.Compound("os_file_name", domain_path_var, self.__domainPath).post_goal()
        pyclp.Compound("compile", domain_path_var).post_goal()
        if self.__proceduresPath is not None:
            pyclp.Compound("os_file_name", procedures_def_path_var, self.__proceduresPath).post_goal()
            pyclp.Compound("compile", procedures_def_path_var).post_goal()
        result, dummy = pyclp.resume()
        msg = ""
        if result == pyclp.FLUSHIO:
            stdout = pyclp.Stream(dummy)
            msg = stdout.readall()
            #result, dummy = pyclp.resume()   
        if result != pyclp.SUCCEED:
            raise(SALMAException("Can't compile Eclipse CLP progression module (msg = {})".format(msg)))
        
        self.reset()
        
        # todo: load fluent and action description
    
    
    
    def printToplevelGoals(self):    
        #v = pyclp.Var()
        goal = pyclp.Atom("print_toplevel_goals")
        goal.post_goal()
        result, stream_num = pyclp.resume()
        
        lines = []
        outstream = None
        while result == pyclp.FLUSHIO:
            if outstream == None:
                outstream = pyclp.Stream(stream_num)           
            
            line = outstream.readall()
            lines.append(line.decode())
            result, _ = pyclp.resume()
        if outstream != None:
            outstream.close()
        return lines
                  
    def __callGoal(self, goalName, *params, **kwargs):
        """
        Calls the goal with the given name and params.
        :param goalName:
        :param params:
        :param kwargs:
        :returns: (goal, result, msg)
         :rtype: (pyclp.Compound, int, str)
        """
        if len(params) == 0:
            goal = pyclp.Atom(goalName)
        else:
            goal = pyclp.Compound(goalName, *params)
        
        goal.post_goal()
        
        result, stream_num = pyclp.resume()
        
        errorMsg = kwargs['errorMsg'] if 'errorMsg' in kwargs else "Error calling {}({}) (result: {}, msg: {})"  
            
        
        lines = []
        outstream = None
        while result == pyclp.FLUSHIO:
            if outstream == None:
                outstream = pyclp.Stream(stream_num)           
            
            line = outstream.readall()
            lines.append(line.decode())
            result, _ = pyclp.resume()
        if outstream != None:
            outstream.close()   
        msg = "\n".join(lines)
        if result != pyclp.SUCCEED:
            readableParams = []
            for p in params:
                if p is not None:
                    try:
                        readableParams.append(self.__convert_value_from_engine_result(p))
                    except:
                        moduleLogger.warn("Can't convert parameter in __callGoal.")
                else:
                    readableParams.append("None")
                     
                              
            raise(SALMAException(
                errorMsg.format(goalName, readableParams, result, msg)))
        return (goal, result, msg)
        
        
    
    
    def reset(self, retractDomains = True, removeFormulas = True, deleteConstants = True):
        
        self.__currentState = None
        self.__properties = dict()
       
        a = pyclp.Var()
        b = pyclp.Var()
        
        if deleteConstants:
            self.__constants = dict()
        if retractDomains:
            self.__callGoal("retractall", 
                            pyclp.Compound("domain", a, b))
        if removeFormulas:
            self.__callGoal("init_agasmc", erorMsg = "Can't initialize main module.") 
        else:
            self.__callGoal("reset_agasmc", erorMsg = "Can't reset main module.")    
       
        self.__callGoal("init_domaindesc", erorMsg = "Can't initialize domain description.")

    def __convert_value_from_engine_result(self, raw_value):
        """
        Converts a value from ECLiPSe to Python.

        :param raw_value: the value that was returned by a PyCLP goal call.

        - Number -> Number
        - Compound  x:y -> (x,y)
        - Compound functor(x,y) -> ('functor', x, y)
        """
        if isinstance(raw_value, pyclp.Var):
            return self.__convert_value_from_engine_result(raw_value.value())
        elif isinstance(raw_value, pyclp.Compound):
            # handle name : type arguments
            subterms = []
            if raw_value.functor() != ':':
                subterms.append(raw_value.functor())
            for subterm in raw_value:
                subterms.append(self.__convert_value_from_engine_result(subterm))
            return tuple(subterms)
        elif isinstance(raw_value, numbers.Number):
            return raw_value
        elif isinstance(raw_value, (pyclp.PList, list, tuple)):
            result = []
            for e in raw_value:
                result.append(self.__convert_value_from_engine_result(e))
            return result
        else:
            result = str(raw_value)
            if result == "true": 
                return True
            elif result == "false":
                return False
            else:
                return result

    def __updateCurrentState(self):
        self.__currentState = dict()      
        
        a = pyclp.Var()
        b = pyclp.Var()
        
        self.__callGoal("current_state", a, b)    
        
        for x in b.value():
            fluentName = str(x[0][0])
            
            fluentParams = []
           
            for j in range(1, x[0].arity()):
                fluentParams.append(self.__convert_value_from_engine_result(x[0][j]))
                
            fluentValue =  self.__convert_value_from_engine_result(x[1])
            fv = FluentValue(fluentName, fluentParams, fluentValue)
            self.__currentState[(fluentName, tuple(fluentParams))] = fv    
        
    
    def getCurrentState(self):
        '''
        Returns a list of engine.FluentValue instances that contain the current state.
        '''
        if self.__currentState is None:
            self.__updateCurrentState()
               
        return self.__currentState.values()
               
    def restoreState(self, fluentValues):
        for fv in fluentValues:
            self.setFluentValue(fv.fluentName, fv.fluentParamValues, fv.value)
        
             
    def getFluentValue(self, fluentName, *fluentParams):
        if self.__currentState is None:
            self.__updateCurrentState()
            
        key = (fluentName, fluentParams)
        if not key in self.__currentState:
            return None
        else: 
            return self.__currentState[key] 
    
    def setFluentValue(self, fluentName, fluentParams, value):
        self.__currentState = None
        pterms = createParamTerms(*fluentParams)
        vterm = createParamTerms(value)[0]
        self.__callGoal("set_current",
                           pyclp.Atom(fluentName),
                           pyclp.PList(pterms),
                           vterm)
                        
    def cleanup(self):
        pyclp.cleanup()

    # TODO: refactor to handle multiple actions simultaneously
    # actions: list of tuples: (action_name:string, parameters:list) 
    def progress(self, actions):
        
        self.__currentState = None # invalidate cache
        actionTerms = []
        for action in actions:
            actionTerm = createTerm(action[0], *action[1])
            actionTerms.append(actionTerm)
        
        failedActions = pyclp.Var()
        goal, _, _ = self.__callGoal("progress_sequential", pyclp.PList(actionTerms), failedActions)
        
        
        return EclipseCLPEngine.__translateFailedActions(failedActions.value())
    
    @staticmethod
    def __translateFailedActions(actionTerms):
        result = []
        for actionTerm in actionTerms:
            if isinstance(actionTerm, pyclp.Atom):
                result.append((str(actionTerm),[]))
            else:
                # compund term
                actionName = str(actionTerm.functor())
                params = [str(p) for p in actionTerm.arguments()]
                result.append((actionName, params))
        return result
        
    def evaluateCondition(self, conditionGoalName, *conditionGoalParams, **kwargs):      
        paramTerms = createParamTerms(*conditionGoalParams, **kwargs)

        goal = pyclp.Compound('evaluate_condition', pyclp.Atom(conditionGoalName), pyclp.PList(paramTerms))        
             
        goal.post_goal()
        
        result, stream_num = pyclp.resume()
        
        lines = []
        outstream = None
        while result == pyclp.FLUSHIO:
            if outstream == None:
                outstream = pyclp.Stream(stream_num)           
            
            line = outstream.readall()
            lines.append(line.decode())
            result, _ = pyclp.resume()
        if outstream != None:
            outstream.close()
        
        
        if result == pyclp.SUCCEED:
            return True
        else: 
            return False
        
        
    def evaluateFunctionGoal(self, goalName, *goalParams, **kwargs):
        params = list(goalParams)
        rvar = pyclp.Var()
        
        paramTerms = createParamTerms(*params, **kwargs)
        self.__callGoal('evaluate_function', pyclp.Atom(goalName), pyclp.PList(paramTerms), rvar)
        
        return rvar.value()
               
          
    def evaluateRelationalGoal(self, situation, goalName, *goalParams):
        # TODO: implement
        pass    
    
    
    
    
    def __prepareIndexedFreeParams(self, *params, **kwargs):
        params2 = []
        indices = dict()
        variables = []
        
        # pyclp.Var is unhashable so we have to use indices to store names
        i = 0
        for param in params:
            if isinstance(param, tuple):
                newVar = pyclp.Var()
                params2.append((':', newVar, param[1]))
                variables.append(newVar) 
                indices[param[0]] = i 
                i += 1  
            else:
                params2.append(param)
        
        paramTerms = createParamTerms(*params2, **kwargs)
        return (variables, indices, paramTerms)
    
    
    def __prepareSelectEntities(self, predicateName, *params, **kwargs):
        variables, indices, paramTerms = self.__prepareIndexedFreeParams(
                                                        *params,
                                                        **kwargs)
        
        g = pyclp.Compound("select_entities", 
                       pyclp.Atom(predicateName), 
                       pyclp.PList(paramTerms)
                       )
        return (variables, indices, g)
        
    
    def __readPyCLPOutputLines(self, result, stream_num):
        outstream = None
        lines = []
        while result == pyclp.FLUSHIO:
            if outstream == None:
                outstream = pyclp.Stream(stream_num)           
            
            line = outstream.readall()
            lines.append(line.decode())
            result, _ = pyclp.resume()
        if outstream != None:
            outstream.close()  
        return result, lines
    
    def selectAll(self, predicateName, *params, **kwargs):
        
        variables, indices, selectCall = self.__prepareSelectEntities(predicateName, *params, **kwargs)
                       
        resultList = pyclp.Var()
             
        varSeq = pyclp.Compound('val', *variables)
        g = pyclp.Compound("findall", 
                           varSeq,
                           selectCall,
                           resultList)        
        g.post_goal()
        result, stream_num = pyclp.resume()
        
        errorMsg = "Can't execute findall for predicate {}. Result = {}, Message = {}."   
      
        result, lines = self.__readPyCLPOutputLines(result, stream_num)
            
        if result != pyclp.SUCCEED:
            raise(SALMAException(
                errorMsg.format(predicateName, result, "\n".join(lines))))
        
        refinedResult = []
        for valueCombination in resultList.value():
            entry = dict()
            for varName, index in indices.items():
                entry[varName] = self.__convert_value_from_engine_result(valueCombination[index])
            refinedResult.append(entry)
        return refinedResult

    def selectFirst(self, predicateName, *params, **kwargs):
        variables, indices, selectCall = self.__prepareSelectEntities(predicateName, *params, **kwargs)
        selectCall.post_goal()
        result, stream_num = pyclp.resume()
        result, lines = self.__readPyCLPOutputLines(result, stream_num)
        
        if result != pyclp.SUCCEED:
            if result == pyclp.THROW:
                errorMsg = "Can't execute predicate {}. Message = {}."
                raise(SALMAException(
                                   errorMsg.format(predicateName, "\n".join(lines))))
            else:
                return None

        pyclp.cut() # throw away all but the first result
        entry = dict()
        for varName, index in indices.items():
            entry[varName] = self.__convert_value_from_engine_result(variables[index].value())
        
        
        return entry       

    def createPlan(self, procedureName, *params, **kwargs):
        variables, indices, paramTerms = self.__prepareIndexedFreeParams(
                                                        *params,
                                                        **kwargs)
        
        plan = pyclp.Var()
         
        g = pyclp.Compound("create_procedure_plan",
                           pyclp.Atom(procedureName),
                           pyclp.PList(paramTerms),
                           plan)
        
        g.post_goal()
        
        result, stream_num = pyclp.resume()
        result, lines = self.__readPyCLPOutputLines(result, stream_num)
        
        if result != pyclp.SUCCEED:
            if result == pyclp.THROW:
                errorMsg = "Can't create plan for procedure {}. Message = {}."
                raise(SALMAException(
                                   errorMsg.format(procedureName, "\n".join(lines))))
            else:
                return (None, None)
        
        pyclp.cut() # throw away all but the first result
        valueCombination = dict()
        for varName, index in indices.items():
            valueCombination[varName] = self.__convert_value_from_engine_result(variables[index].value())

        translatedPlan = []
        #: :type action: pyclp.Compound
        for action in plan.value():
            actionName = action.functor()
            actionParams = []
            for ap in action:
                actionParams.append(self.__convert_value_from_engine_result(ap))
            
            translatedPlan.append(tuple([actionName] + actionParams))            
        
        return (translatedPlan, valueCombination)        

    def defineDomain(self, sortName, objectIds):
        self.__currentState = None
        
        refinedEntities = createParamTerms(*objectIds)
        
        self.__callGoal("setDomain",
                        pyclp.Atom(sortName),
                        pyclp.PList(refinedEntities), 
                        errorMsg = "Error defining domain for sort %s" % sortName)

    def initSortHierarchy(self):
        domVar = pyclp.Var()
        self.__callGoal('init_sort_hierarchy', domVar,
                        errorMsg = "Error while initializing sort hierarchy.")
        
        #: :type domList: pyclp.PList
        domList = domVar.value()
        result = dict()
        #: :type dom: pyclp.Compound
        for dom in domList:
            sort = self.__convert_value_from_engine_result(dom[0])
            entities = self.__convert_value_from_engine_result(dom[1])
            result[sort] = entities
        return result
        
    def setConstantValue(self, constantName, constantParams, value):
        # remember: the constant has to be defined as dynamic in Eclipse
        # the constant value is kept in "python space"
        self.__currentState = None
        pterms = createParamTerms(*constantParams)
        vterm = createParamTerms(value)[0]
        self.__callGoal("setConstant", pyclp.Atom(constantName), 
                        pyclp.PList(pterms + [vterm]))
                               
        self.__constants[(constantName, tuple(constantParams))] = value

    def __retrieve_constant_value(self, constant_name, constant_params):
        pterms = createParamTerms(*constant_params)
        val = pyclp.Var()
        pterms.append(val)
        goal = pyclp.Compound(constant_name, *pterms)
        goal.post_goal()
        result, _ = pyclp.resume()
        value = None
        if result == pyclp.SUCCEED and val.value() is not None:
            value = self.__convert_value_from_engine_result(val.value())
            self.__constants[(constant_name, tuple(constant_params))] = value
        return value

    def getConstantValue(self, constantName, constantParams):
        v = None
        try:
            v = self.__constants[(constantName, tuple(constantParams))]
        except KeyError:
            v = self.__retrieve_constant_value(constantName, constantParams)

        return v

    def isConstantDefined(self, constantName, constantParams):
        return (constantName, tuple(constantParams)) in self.__constants
                     
    def evaluationStep(self):
        overallVerdict = pyclp.Var()
        toplevel_results = pyclp.Var()
        scheduled_results = pyclp.Var()
        
        self.__callGoal('evaluation_step', overallVerdict, toplevel_results, scheduled_results)
        
        return (EclipseCLPEngine.__verdictMapping[str(overallVerdict.value())], 
                EclipseCLPEngine.__translateToplevelEvaluationResults(toplevel_results.value()), 
                EclipseCLPEngine.__translateScheduledEvaluationResults(scheduled_results.value())
                )    
    
    @staticmethod
    def __translateToplevelEvaluationResults(result):
        resultDict = dict()
        if isinstance(result, pyclp.PList):
            for r in result:
                resultDict[str(r[1])] = EclipseCLPEngine.__verdictMapping[str(r[0])]
        return resultDict

    @staticmethod
    def __translateScheduledEvaluationResults(result):
        resultDict = dict()
        if isinstance(result, pyclp.PList):
            for r in result:
                #not_ok : sg(ToplevelFormula, Level, ScheduleIdOut, CurrentTime)
                verdict = EclipseCLPEngine.__verdictMapping[str(r[0])]
                key = r[1]
                resultDict[(str(key[0]), key[3])] = verdict
            
        return resultDict

    def registerProperty(self, propertyName, formula):
        compiledFormula = pyclp.Var()
        self.__callGoal('register_property_str', pyclp.Atom(propertyName), formula, compiledFormula)
        self.__properties[propertyName] = formula
        
    def getProperties(self):
        return self.__properties.copy()

    def getExogenousActionCandidates(self):
        """
        Returns a list of form [action_name : [ [x1_1, x2_1, ...], [x1_2, x2_2, ...], ...], actionName2 : ... ]
        """
        candidates = pyclp.Var()
        self.__callGoal('get_all_exogenous_action_instances', candidates)
        result = dict()
        for actionDef in candidates.value():
            actionName = str(actionDef[0])
            candidates = actionDef[1] #    [ [x1_1, x2_1, ...], [x1_2, x2_2, ...], ...]     
            instances = []
            for c in candidates:
                instance = []
                for arg in c:
                    if isinstance(arg, pyclp.Atom):
                        instance.append(str(arg))
                    else:
                        instance.append(arg)
                
                instances.append(instance)
                
            result[actionName] = instances
            
        return result

    def getActionClock(self, actionName, params):
        pterms = createParamTerms(params)
        t = pyclp.Var()
        self.__callGoal('get_action_clock',
                        pyclp.Atom(actionName),
                        pyclp.PList(pterms),
                        t)
        
        return self.__convert_value_from_engine_result(t.value())

    def getFluentChangeTime(self, fluentName, params):
        pterms = createParamTerms(params)
        t = pyclp.Var()
        self.__callGoal('get_last_change_time',
                        pyclp.Atom(fluentName),
                        pyclp.PList(pterms),
                        t)
        
        return self.__convert_value_from_engine_result(t.value())

    def queryPersistentProperty(self, propertyName):
        rawStatus = pyclp.Var()
        rawT = pyclp.Var()
        self.__callGoal('query_persistent_fluent',
                        pyclp.Atom(propertyName),
                        rawStatus,
                        rawT)
        s = self.__verdictMapping[str(rawStatus.value())]
        time = self.__convert_value_from_engine_result(rawT.value())
        if s == OK:
            status = True
        elif s == NOT_OK:
            status = False
        else:
            raise SALMAException(
                    "Wrong status returned for persistent property {}: {}".format(
                                                                                  propertyName,
                                                                                  str(rawStatus.value())))
        return (status, time)

    def __load_declaration(self, load_function):
        """
        :rtype : list
        """
        entries = pyclp.Var()
        self.__callGoal(load_function, entries)
        converted_entries = []

        for entry in entries.value():
            args = []
            for arg in entry:
                args.append(self.__convert_value_from_engine_result(arg))
            converted_entries.append(tuple(args))

        return converted_entries

    def load_declarations(self):
        fluents = self.__load_declaration('get_declared_fluents')
        constants = self.__load_declaration('get_declared_constants')
        primitive_actions = self.__load_declaration('get_declared_primitive_actions')
        stochastic_actions = self.__load_declaration('get_declared_stochastic_actions')
        exogenous_actions = self.__load_declaration('get_declared_exogenous_actions')
        immediate_actions = list(map(lambda e : e[0],
                                self.__load_declaration('get_declared_immediate_actions')))
        return {'fluents' : fluents,
                'constants' : constants,
                'primitive_actions' : primitive_actions,
                'stochastic_actions' : stochastic_actions,
                'exogenous_actions' : exogenous_actions,
                'immediate_actions' : immediate_actions
                }

__all__ = ["Engine", "EclipseCLPEngine", "FluentValue", "createParamTerms", "createTerm"]


