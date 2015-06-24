import logging
import numbers
import os

import pyclp

from salma.SALMAException import SALMAException
from .constants import *
import salma
import salma.termutils
from salma.model.data import Term
from salma.termutils import tuplify
from salma.formatutils import format_term

MODULE_LOGGER_NAME = 'salma.engine'
moduleLogger = logging.getLogger(MODULE_LOGGER_NAME)


class FluentValue(object):
    """
    Stores the string representation of a fluent value together with its associated parameters.
    Note that no type is stored here.
    """

    def __init__(self, fluentName, fluentParamValues, value):
        """
        fluentName: fluent name
        fluentParamValues: list of params
        value: fluent value in string form
        """
        self.__fluentName = fluentName
        self.__fluentParamValues = fluentParamValues
        self.__value = value

    def getFluentName(self):
        return self.__fluentName

    fluentName = property(getFluentName)

    def getFluentParamValues(self):
        return self.__fluentParamValues

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
    An interface for engine that connect the core simulation engine to the logics evalauation mechanism.
    """

    def getCurrentState(self):
        """
        returns a list of FluentValue objects.
        :rtype: list[FluentValue]
        """
        raise NotImplementedError()

    def restore_state(self, fluent_values):
        """
        Uses the given list of fluent values to update the world state.
        :param list[FluentValue] fluent_values: the fluent values that define the state
        """
        raise NotImplementedError()

    def getFluentValue(self, fluentName, *fluentParams):
        """
        returns: the FluentValue object
        """
        raise NotImplementedError()

    def get_derived_fluent_value(self, fluent_name, fluent_params):
        """
        Retrieves the value of the given derived fluent instance.
        :param str fluent_name:
        :param list|tuple fluent_params: params
        :return: object
        """
        raise NotImplementedError()

    def setFluentValue(self, fluentName, fluentParams, value):
        raise NotImplementedError()

    def cleanup(self):
        """
        Resets the values of all fluents.
        """
        raise NotImplementedError()

    def progress(self, actions):
        """
        Performs a progression of all fluents with the given actions.
        """
        raise NotImplementedError()

    def evaluateCondition(self, conditionGoalName, *conditionGoalParams, **kwargs):
        """
        Evaluates the given goal and returns true if the evaluation succeeds.

        conditionGoalName: name of the condition goal. The goal has to be defined in the domain module.
        conditionGoalParams: the parameters that are used for evaluating the condition goal.
        returns: true if evaluation succeeded
        """
        raise NotImplementedError()

    def evaluateFunctionGoal(self, goalName, *goalParams, **kwargs):
        """
        Evaluates the given goal with the given parameters and an implicit last parameter for the result variable.
        Only the first result is returned.

        If a situation is given as named argument "situation", it is appended after the result variable at the last
        position.

        :param str goalName: the name of the Prolog goal to evaluate
        :param goalParams: the arguments for evaluating the given goal.
        """
        raise NotImplementedError()

    def selectAll(self, predicateName, *params, **kwargs):
        """
        returns a list of tuples with all possible value combinations for the given variables that make
        the execution of the given predicate
        succeed

        :param predicateName str:
        :param params list: list of either concrete values or (name, type) tuples
        :rtype list: list of dicts
        """
        raise NotImplementedError()

    def selectFirst(self, predicateName, *params, **kwargs):
        """
        returns a tuple with the first possible value combination that make the execution of the given predicate
        succeed

        :param params list: list of either concrete values or (name, type) tuples
        :param predicateName str:
        :rtype dict:
        """
        raise NotImplementedError()

    def createPlan(self, procedureName, *params, **kwargs):
        """
        Creates a plan for the given procedure.

        returns a tuple: (plan = list of ground ActionExecutions, dict with params)
        """
        raise NotImplementedError()

    def reset(self, erase_properties=False):
        raise NotImplementedError()

    def defineDomain(self, sortName, objectIds):
        """
        Defines the domain of the given sort to be the given list of objectIds

        :param sortName: str
        :param objectIds: list
        """
        raise NotImplementedError()

    def initSortHierarchy(self):
        """
        Resolves all declared sort - subsort relationships and initializes supersorts
        accordingly.

        """
        raise NotImplementedError()

    def setConstantValue(self, constantName, constantParams, value):
        """
        sets the given constant value for the given params

        The value is cached and all subsequent calls to getConstantValue() will return
        the cached value.

        :param constantName: str
        :param constantParams: list
        :param value: object
        """
        raise NotImplementedError()

    def getConstantValue(self, constantName, constantParams):
        """
        Returns the cached value for the given constant with the given parameters together with a flag that
        tells whether the value is defined, i.e. tuples of the form (defined, value).

        :param constantName: str
        :param constantParams: list
        :rtype: (bool, object)
        """
        raise NotImplementedError()

    def isConstantDefined(self, constantName, constantParams):
        """
        Returns true iff a value is defined for the given constant with the given parameters.

        :param constantName: str
        :param constantParams: list
        """
        raise NotImplementedError()

    def evaluationStep(self, interval_end=None):
        """
        returns a tuple with two dicts: (toplevel_results, scheduled_results, scheduled_goals, failure_stack)
        toplevel_results: propertyName : verdict (OK, NOT_OK, NONDET)
        scheduled_results: (propertyName, time) : verdict
        :param int interval_end : the end time of the inspected interval
        :rtype: (dict[str, int], dict, list, list)
        """
        raise NotImplementedError()

    def format_failure_stack(self, failure_stack):
        """
        Returns a formatted string representation of a failure trace as it way returned by evaluationStep().
        :param list failure_stack: the failure stack to format.
        :rtype: str
        """
        raise NotImplementedError()

    def registerProperty(self, propertyName, formula):
        """
        Registers the given formula under the given name.
        """
        raise NotImplementedError()

    def printToplevelGoals(self):
        raise NotImplementedError()

    def getProperties(self):
        """
        Returns a dict with name : formula
        """
        raise NotImplementedError()

    def get_next_possible_ad_hoc_event_instances(self, start, time_limit, handled_events):
        """
        Searches for the next time step where a poss-axiom of an exogenous action becomes true. Returns that time
        together with all possible exogenous action instances at that situation.

        :param int start: the time of the step when scanning should be started .
        :param int time_limit: the time step until which the search should proceed. This is meant to be the next time
        step that is known to be inspected.
        :param list[(int, str, tuple)] handled_events: the event instances that have already been handled in this time
            step
        :return: a list of tuples (time, action_name, arguments)
        :rtype: list[(int, str, tuple)]
        """
        raise NotImplementedError()

    def get_next_schedulable_event_instances(self, start, time_limit, current_schedule, handled_events):
        """
        Searches for the next time step where a schedulability axiom of an exogenous action becomes true. Returns that
        time together with all schedulable exogenous action instances at that situation.

        :param int time_limit: the time step until which the search should proceed. This is meant to be the next time
                    step that is known to be inspected.
        :param list[(int, str, tuple)] current_schedule: the currently scheduled event instances as tuples of form
                                                            (time, action_name, params)
        :param list[(int, str, tuple)] handled_events: the event instances that have already been handled in this time
            step
        :return: a list of tuples (time, action_name, arguments)
        :rtype: list[(int, str, tuple)]
        """
        raise NotImplementedError()

    def getActionClock(self, actionName, params):
        """
        Returns the last recorded time when the given action was galled with the given parameters.

        If the given action-parameter combination hasn't occurred before, this method returns -1.

        :type actionName: str
        :type params: list|tuple

        :rtype: int
        """
        raise NotImplementedError()

    def getFluentChangeTime(self, fluentName, params):
        """
        Returns the last recorded time when the given fluent with the given parameters.

        If the given fluent-parameter combination has not been initialized yet,
        this method returns -1.

        :param fluentName: str
        :param params: list
        """
        raise NotImplementedError()

    def queryPersistentProperty(self, propertyName):
        """
        Returns a tuple with the current status of the given property together with the last
        change time.

        :param propertyName: str
        :rtype: tuple
        """
        raise NotImplementedError()

    def load_declarations(self):
        """
        Compiles all declarations of fluents, constants and actions from the ECLiPSe model. Returns a
        dict with the category as key and a list of tuples as value. The keys are:
        fluents, constants, primitive_actions, stochastic_actions, exogenous_actions.

        :rtype: dict
        """
        raise NotImplementedError()

    def create_message(self, connector, agent, msg_type, params):
        """
        Creates and returns a new message for the given channel, the given agent, and the given parameters. The
        message type can be one of "unicast", "multicastSrc" and "sensor".

        A new "virtual" object of sort "message" is created and its id returned. The message
         is added to the domain of sort message. Additionally, the constant message_spec is set with the term
         msg(Con, MsgType, Agent, Params).

        :type connector: str
        :type agent: str
        :type msg_type: str
        :type params: list
        :rtype: int
        """
        raise NotImplementedError()


# TODO: document conversion behavior in general docs
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
        # term : object
        if p is None:
            term = pyclp.Atom('none')
        elif isinstance(p, bool):
            term = pyclp.Atom('true') if p is True else pyclp.Atom('false')
        elif isinstance(p, numbers.Number):
            if p == 0:
                term = pyclp.Atom('zero')
            else:
                term = p
        elif isinstance(p, pyclp.Var):
            term = p
        elif isinstance(p, Term):
            term = pyclp.Compound(p.functor, *createParamTerms(*p.params))
        elif isinstance(p, (tuple, list)):
            subterms = []
            for subterm in p:
                subterms.append(createParamTerms(subterm)[0])
            term = pyclp.PList(subterms)
        else:
            term = pyclp.Atom(str(p))
        paramTerms.append(term)
    situation = kwargs['situation'] if 'situation' in kwargs else None
    if situation is not None:
        paramTerms.append(pyclp.Atom(situation))

    return paramTerms


def create_term(functor, *params, **kwargs):
    param_terms = createParamTerms(*params, **kwargs)

    if len(param_terms) == 0:
        return pyclp.Atom(functor)
    else:
        return pyclp.Compound(functor, *param_terms)  # must unpack because otherwise the term would contain brackets


class EclipseCLPEngine(Engine):
    """
    The Engine implementation for ECLiPSe.
    """
    # TODO: make this somehow more module relative
    PROGRESSION_MODULE = os.path.abspath(
        os.path.join(salma.__path__[0], "../ecl-src/agasmc.ecl"))

    __verdictMapping = {'ok': OK, 'not_ok': NOT_OK, 'nondet': NONDET, 'ambiguous': AMBIGUOUS}

    __pyclp_initialized = False

    def __init__(self, domain_path, procedure_def_path=None, domain_init_function=None):
        # make absolute path

        self.__domainPath = os.path.abspath(domain_path)

        self.__proceduresPath = None if procedure_def_path is None else os.path.abspath(procedure_def_path)
        self.__domain_init_function = domain_init_function
        self.__properties = dict()
        self.__constants = dict()

        # dict: (fluentName, tuple(fluentParams)) -> engine.FluentValue
        self.__currentState = None
        if EclipseCLPEngine.__pyclp_initialized:
            pyclp.cleanup()
            EclipseCLPEngine.__pyclp_initialized = False
        pyclp.init()  # Init ECLiPSe engine
        EclipseCLPEngine.__pyclp_initialized = True

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
            # result, dummy = pyclp.resume()
        if result != pyclp.SUCCEED:
            raise (SALMAException("Can't compile Eclipse CLP progression module (msg = {})".format(msg)))
        self.reset(erase_properties=True)

    def printToplevelGoals(self):
        # v = pyclp.Var()
        goal = pyclp.Atom("print_toplevel_goals")
        goal.post_goal()
        result, stream_num = pyclp.resume()

        lines = []
        outstream = None
        while result == pyclp.FLUSHIO:
            if outstream is None:
                outstream = pyclp.Stream(stream_num)

            line = outstream.readall()
            lines.append(line.decode())
            result, _ = pyclp.resume()
        if outstream is not None:
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

        errorMsg = (kwargs['errorMsg'] if 'errorMsg' in kwargs
                    else "Error calling {}({}) (result: {}, msg: {}, info: {})")
        lines = []
        outstream = None
        while result == pyclp.FLUSHIO:
            if outstream is None:
                outstream = pyclp.Stream(stream_num)

            line = outstream.readall()
            lines.append(line.decode())
            result, _ = pyclp.resume()
        if outstream is not None:
            outstream.close()
        msg = "\n".join(lines)
        if result != pyclp.SUCCEED:
            readableParams = []
            for p in params:
                if p is not None:
                    readableParams.append(self.__convert_value_from_engine_result(p))
                else:
                    readableParams.append("None")

            raise (SALMAException(
                errorMsg.format(goalName, readableParams, result, msg, stream_num)))
        return goal, result, msg

    def reset(self, erase_properties=False):

        self.__currentState = None
        self.__properties = dict()
        self.__constants = dict()

        if erase_properties:
            self.__callGoal("init_agasmc", erorMsg="Can't initialize main module.")
        else:
            self.__callGoal("reset_agasmc", erorMsg="Can't reset main module.")

        if self.__domain_init_function is not None:
            self.__callGoal(self.__domain_init_function, erorMsg="Can't initialize domain description.")

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
            # handle "name : type" arguments specially
            subterms = []

            for subterm in raw_value:
                subterms.append(self.__convert_value_from_engine_result(subterm))
            if raw_value.functor() == ':':
                return tuple(subterms)
            else:
                return Term(raw_value.functor(), *subterms)
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
            elif result.lower() == "none":
                return None
            else:
                return result

    def __updateCurrentState(self, key=None):
        if key is None:
            self.__load_full_current_state()
            return

        if self.__currentState is None:
            self.__currentState = dict()

        fluent_name = key[0]
        params = list(key[1])
        val = pyclp.Var()
        pterms = createParamTerms(*params)
        self.__callGoal("get_fluent_value",
                        pyclp.Atom(fluent_name),
                        pyclp.PList(pterms),
                        val,
                        pyclp.Atom("s0"))
        val2 = self.__convert_value_from_engine_result(val.value())
        fv = FluentValue(fluent_name, params, val2)
        self.__currentState[key] = fv

    def __load_full_current_state(self):
        self.__currentState = dict()
        b = pyclp.Var()

        self.__callGoal("get_all_fluent_values", b, pyclp.Atom("s0"))

        for x in b.value():
            fluentName = str(x[0][0])

            fluentParams = []

            for j in range(1, x[0].arity()):
                fluentParams.append(self.__convert_value_from_engine_result(x[0][j]))

            fluentValue = self.__convert_value_from_engine_result(x[1])
            fv = FluentValue(fluentName, fluentParams, fluentValue)
            self.__currentState[(fluentName, tuple(fluentParams))] = fv

    def getCurrentState(self):
        """
        Returns a list of engine.FluentValue instances that contain the current state.
        """
        self.__updateCurrentState()
        return self.__currentState.values()

    def restore_state(self, fluent_values):
        """
        Uses the given list of fluent values to update the world state.
        :param list[FluentValue] fluent_values: the fluent values that define the state
        """
        self.__currentState = None
        for fv in fluent_values:
            self.setFluentValue(fv.fluentName, fv.fluentParamValues, fv.value)

    def getFluentValue(self, fluentName, *fluentParams):
        key = (fluentName, fluentParams)

        if (self.__currentState is None) or (key not in self.__currentState):
            self.__updateCurrentState(key)
        # self.__updateCurrentState(key)
        if key not in self.__currentState:
            return None
        else:
            return self.__currentState[key]

    def get_derived_fluent_value(self, fluent_name, fluent_params):
        val = pyclp.Var()
        pterms = createParamTerms(*fluent_params)
        self.__callGoal("get_fluent_value",
                        pyclp.Atom(fluent_name),
                        pyclp.PList(pterms),
                        val,
                        pyclp.Atom("s0"))
        val2 = self.__convert_value_from_engine_result(val.value())
        return val2

    def setFluentValue(self, fluentName, fluentParams, value):
        pterms = createParamTerms(*fluentParams)
        vterm = createParamTerms(value)[0]
        self.__callGoal("set_current",
                        pyclp.Atom(fluentName),
                        pyclp.PList(pterms),
                        vterm)
        if self.__currentState is not None:
            fv = FluentValue(fluentName, fluentParams, value)
            self.__currentState[(fluentName, tuple(fluentParams))] = fv

    def cleanup(self):
        pyclp.cleanup()

    # actions: list of tuples: (action_name:string, parameters:list) 
    def progress(self, actions):
        self.__currentState = None  # invalidate cache
        actionTerms = []
        for action in actions:
            actionTerm = create_term(action[0], *action[1])
            actionTerms.append(actionTerm)

        failedActions = pyclp.Var()
        goal, _, _ = self.__callGoal("progress_sequential", pyclp.PList(actionTerms), failedActions)
        # self.__updateCurrentState()
        return EclipseCLPEngine.__translateFailedActions(failedActions.value())

    @staticmethod
    def __translateFailedActions(actionTerms):
        result = []
        for actionTerm in actionTerms:
            if isinstance(actionTerm, pyclp.Atom):
                result.append((str(actionTerm), []))
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
            if outstream is None:
                outstream = pyclp.Stream(stream_num)

            line = outstream.readall()
            lines.append(line.decode())
            result, _ = pyclp.resume()
        if outstream is not None:
            outstream.close()

        if result == pyclp.SUCCEED:
            return True
        else:
            return False

    def evaluateFunctionGoal(self, goalName, *goalParams, **kwargs):
        params = list(goalParams)
        rvar = pyclp.Var()
        params.append(rvar)
        paramTerms = createParamTerms(*params, **kwargs)
        self.__callGoal('evaluate_function', pyclp.Atom(goalName), pyclp.PList(paramTerms))
        return self.__convert_value_from_engine_result(rvar.value())

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
        return variables, indices, paramTerms

    def __prepareSelectEntities(self, predicateName, *params, **kwargs):
        variables, indices, paramTerms = self.__prepareIndexedFreeParams(
            *params,
            **kwargs)

        g = pyclp.Compound("select_entities",
                           pyclp.Atom(predicateName),
                           pyclp.PList(paramTerms))
        return variables, indices, g

    def __readPyCLPOutputLines(self, result, stream_num):
        outstream = None
        lines = []
        while result == pyclp.FLUSHIO:
            if outstream is None:
                outstream = pyclp.Stream(stream_num)

            line = outstream.readall()
            lines.append(line.decode())
            result, _ = pyclp.resume()
        if outstream is not None:
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
            raise (SALMAException(
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
                raise (SALMAException(
                    errorMsg.format(predicateName, "\n".join(lines))))
            else:
                return None

        pyclp.cut()  # throw away all but the first result
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
                raise (SALMAException(
                    errorMsg.format(procedureName, "\n".join(lines))))
            else:
                return None, None

        pyclp.cut()  # throw away all but the first result
        valueCombination = dict()
        for varName, index in indices.items():
            valueCombination[varName] = self.__convert_value_from_engine_result(variables[index].value())

        translatedPlan = []
        # : :type action: pyclp.Compound
        for action in plan.value():
            actionName = action.functor()
            actionParams = []
            for ap in action:
                actionParams.append(self.__convert_value_from_engine_result(ap))

            translatedPlan.append(tuple([actionName] + actionParams))

        return translatedPlan, valueCombination

    def defineDomain(self, sortName, objectIds):
        self.__currentState = None

        refinedEntities = createParamTerms(*objectIds)

        self.__callGoal("setDomain",
                        pyclp.Atom(sortName),
                        pyclp.PList(refinedEntities),
                        errorMsg="Error defining domain for sort %s" % sortName)

    def getAllDomains(self):
        v = pyclp.Var()
        self.__callGoal("get_all_domains", v)
        return self.__convert_value_from_engine_result(v)

    def getFluentValue__direct(self, fluentName, params):
        refinedParams = createParamTerms(*params)
        v = pyclp.Var()
        self.__callGoal("get_current", refinedParams, v)
        return self.__convert_value_from_engine_result(v)

    def initSortHierarchy(self):
        domVar = pyclp.Var()
        self.__callGoal('init_sort_hierarchy', domVar,
                        errorMsg="Error while initializing sort hierarchy.")

        # : :type domList: pyclp.PList
        domList = domVar.value()
        result = dict()
        # : :type dom: pyclp.Compound
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
        """
        Retrieves the value of the given constant from the engine. Returns
        a tuple of form (defined, value).
        :param str constant_name: the constant name
        :param list|tuple constant_params: the constant params
        :rtype: (bool, object)
        """
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
            return True, value
        else:
            return False, None

    def getConstantValue(self, constantName, constantParams):
        v = None
        try:
            d, v = True, self.__constants[(constantName, tuple(constantParams))]
        except KeyError:
            d, v = self.__retrieve_constant_value(constantName, constantParams)
        return d, v

    def isConstantDefined(self, constantName, constantParams):
        return (constantName, tuple(constantParams)) in self.__constants

    def evaluationStep(self, interval_end=None):
        toplevel_results = pyclp.Var()
        scheduled_results = pyclp.Var()
        scheduled_keys = pyclp.Var()
        failure_stack = pyclp.Var()

        if interval_end is None:
            self.__callGoal('evaluation_step', toplevel_results, scheduled_results, scheduled_keys, failure_stack)
        else:
            self.__callGoal('evaluation_step', interval_end, toplevel_results, scheduled_results, scheduled_keys,
                            failure_stack)

        fstack = self.__convert_value_from_engine_result(failure_stack.value())
        if isinstance(fstack, (list, tuple)):
            fstack = list(map(format_term, fstack))
        else:
            fstack = [str(format_term(fstack))]

        return (EclipseCLPEngine.__translateToplevelEvaluationResults(toplevel_results.value()),
                EclipseCLPEngine.__translateScheduledEvaluationResults(scheduled_results.value()),
                EclipseCLPEngine.__translate_scheduled_properties(scheduled_keys.value()),
                fstack)

    def format_failure_stack(self, failure_stack):
        """
        :type failure_stack: list[tuple]
        :rtype: str
        """
        lines = []
        for entry in failure_stack:
            label, result, formula_name, path, time, formula, level = entry
            fterm = salma.termutils.term_from_tuple(formula)

            lines.append("{label} : {result} -- {fname} : {path} : t={time}, l={level} : {fterm}".format(
                label=label, result=result, fname=formula_name, path=str(path), time=str(time), level=str(level),
                fterm=fterm))
        return "\n".join(lines)

    @staticmethod
    def __translateToplevelEvaluationResults(result):
        """
        Translates a toplevel result vector into a dict of the format fname -> OK|NOT_OK|NONDET

        :rtype: dict[str, int]
        """
        resultDict = dict()
        if isinstance(result, pyclp.PList):
            for r in result:
                resultDict[str(r[1])] = EclipseCLPEngine.__verdictMapping[str(r[0])]
        return resultDict

    @staticmethod
    def __translateScheduledEvaluationResults(result):
        """
        Translate scheduled evaluation results to a dict: fname -> list((start, end, verdict))
        :rtype: dict[str, list[(int, int)]]
        """
        # : :type: dict[str, list[(int, int, int)]]
        result_dict = dict()

        for r in result:
            # format: r(ToplevelFormula, Id, [s(Start, End) : Result, ...], OverallResult)
            overall_verdict = EclipseCLPEngine.__verdictMapping[str(r[3])]
            pname = str(r[0])
            pid = r[1]
            res_intervals = []
            for ri in r[2]:
                start, end = ri[0]
                verdict_raw = ri[1]
                res_intervals.append((start, end, EclipseCLPEngine.__verdictMapping[str(verdict_raw)], pid))

            if pname in result_dict:
                result_dict[pname].extend(res_intervals)
                result_dict[pname].sort(key=lambda x: x[0])
            else:
                result_dict[pname] = res_intervals

        return result_dict

    @staticmethod
    def __translate_scheduled_properties(props):
        """
        Translates a list of scheduled property keys to a dict: fname -> list(time)
        :param list[pyclp.Compound] props: the list of scheduled properties
        :rtype: dict[str, list[int]]
        """
        # : :type: dict[str, list[int]]
        result_dict = dict()
        for prop in props:
            #  [p(f, id, [s(0, 1), ...])]
            pname = str(prop[0])
            pid = prop[1]
            intervals = []
            for i in prop[2]:
                start, end = i
                intervals.append((start, end, pid))

            if pname in result_dict:
                result_dict[pname].extend(intervals)
                result_dict[pname].sort(key=lambda x: x[0])
            else:
                result_dict[pname] = intervals

        return result_dict

    def registerProperty(self, propertyName, formula):
        compiledFormula = pyclp.Var()
        self.__callGoal('register_property_str', pyclp.Atom(propertyName), formula, compiledFormula)
        self.__properties[propertyName] = formula

    def getProperties(self):
        return self.__properties.copy()

    def get_next_possible_ad_hoc_event_instances(self, start, time_limit, handled_events):
        """
        Searches for the next time step where a poss-axiom of an exogenous action becomes true. Returns that time
        together with all possible exogenous action instances at that situation.

        :param int start: the time of the step when scanning should be started .
        :param int time_limit: the time step until which the search should proceed. This is meant to be the next time
        step that is known to be inspected.
        :param list[(int, str, tuple)] handled_events: the event instances that have already been handled in this time
            step
        :return: a list of tuples (time, action_name, arguments)
        :rtype: list[(int, str, tuple)]
        """

        translated_handled_events = []
        for ev in handled_events:
            evterm = create_term("ev", ev[0], ev[1], ev[2])
            translated_handled_events.append(evterm)

        next_time = pyclp.Var()
        event_candidates = pyclp.Var()
        self.__callGoal('get_next_possible_ad_hoc_events', start, time_limit,
                        translated_handled_events, next_time, event_candidates)

        result = []
        for actionDef in event_candidates.value():
            action_name = str(actionDef[0])
            instance_candidates = actionDef[1]  # [ [x1_1, x2_1, ...], [x1_2, x2_2, ...], ...]
            for c in instance_candidates:
                instance_params = []
                for arg in c:
                    if isinstance(arg, pyclp.Atom):
                        instance_params.append(str(arg))
                    else:
                        instance_params.append(arg)
                result.append((next_time.value(), action_name, tuplify(instance_params)))
        return result

    def get_next_schedulable_event_instances(self, start, time_limit, current_schedule, handled_events):
        """
        Searches for the next time step where a schedulability axiom of an exogenous action becomes true. Returns that
        time together with all schedulable exogenous action instances at that situation.

        :param int time_limit: the time step until which the search should proceed. This is meant to be the next time
                    step that is known to be inspected.
        :param list[(int, str, tuple)] current_schedule: the currently scheduled event instances as tuples of form
                                                            (time, action_name, params)
        :param list[(int, str, tuple)] handled_events: the event instances that have already been handled in this time
            step
        :return: a list of tuples (time, action_name, arguments)
        :rtype: list[(int, str, tuple)]
        """
        translated_schedule = []
        for ev in current_schedule:
            evterm = create_term("ev", ev[0], ev[1], ev[2])
            translated_schedule.append(evterm)

        translated_handled_events = []
        for ev in handled_events:
            evterm = create_term("ev", ev[0], ev[1], ev[2])
            translated_handled_events.append(evterm)

        next_time = pyclp.Var()
        event_candidates = pyclp.Var()

        self.__callGoal('get_next_schedulable_events', start,
                        time_limit, translated_schedule, translated_handled_events, next_time, event_candidates)

        result = []
        for actionDef in event_candidates.value():
            action_name = str(actionDef[0])
            instance_candidates = actionDef[1]  # [ [x1_1, x2_1, ...], [x1_2, x2_2, ...], ...]
            for c in instance_candidates:
                instance_params = []
                for arg in c:
                    if isinstance(arg, pyclp.Atom):
                        instance_params.append(str(arg))
                    else:
                        instance_params.append(arg)
                result.append((next_time.value(), action_name, tuplify(instance_params)))
        return result

    def getActionClock(self, actionName, params):
        pterms = createParamTerms(*params)
        t = pyclp.Var()
        self.__callGoal('get_action_clock',
                        pyclp.Atom(actionName),
                        pyclp.PList(pterms),
                        t)
        return self.__convert_value_from_engine_result(t.value())

    def getFluentChangeTime(self, fluentName, params):
        pterms = createParamTerms(*params)
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
        return status, time

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
        derived_fluents = self.__load_declaration('get_declared_derived_fluents')
        constants = self.__load_declaration('get_declared_constants')
        primitive_actions = self.__load_declaration('get_declared_primitive_actions')
        stochastic_actions = self.__load_declaration('get_declared_stochastic_actions')
        exogenous_actions = self.__load_declaration('get_declared_exogenous_actions')
        exogenous_action_choices = self.__load_declaration("get_declared_exogenous_action_choices")

        channels = self.__load_declaration("get_declared_channels")
        sensors = self.__load_declaration("get_declared_sensors")
        remote_sensors = self.__load_declaration("get_declared_remote_sensors")

        return {'fluents': fluents,
                'derived_fluents': derived_fluents,
                'constants': constants,
                'primitive_actions': primitive_actions,
                'stochastic_actions': stochastic_actions,
                'exogenous_actions': exogenous_actions,
                'exogenous_action_choices': exogenous_action_choices,
                'channels': channels,
                'sensors': sensors,
                'remote_sensors': remote_sensors}

    def evaluate_ad_hoc(self, formula):
        """
        Evaluates the given formula.
        :param str formula: the formula to evaluate
        :rtype: int
        """
        result = pyclp.Var()
        sit = pyclp.Atom("s0")
        self.__callGoal("evaluate_ad_hoc_str", formula, result, sit)
        return EclipseCLPEngine.__verdictMapping[str(result.value())]

    def create_message(self, connector, agent, msg_type, params):
        """
        See documentation in superclass.

        :type connector: str
        :type agent: str
        :type msg_type: str
        :type params: list
        :rtype: int
        """
        # In ECL: create_message(Con, Agent, MsgType, Params, Msg)
        pterms = createParamTerms(*params)
        msgid = pyclp.Var()
        self.__callGoal("create_message", pyclp.Atom(str(connector)), pyclp.Atom(str(agent)),
                        pyclp.Atom(msg_type), pyclp.PList(pterms), msgid)

        return self.__convert_value_from_engine_result(msgid.value())


__all__ = ["Engine", "EclipseCLPEngine", "FluentValue", "createParamTerms", "create_term"]


