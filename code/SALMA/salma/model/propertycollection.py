from salma.SALMAException import SALMAException
from salma.constants import *
from salma.engine import Engine


class PropertyCollection:
    def __init__(self, logics_engine):
        """
        Creates an instance of the property evaluator.

        :param Engine logics_engine: the logics engine to use
        """
        self.__logics_engine = logics_engine

        # dict with registered properties: name -> (formula, property_type)
        # : :type: dict[str, (str, int)]
        self.__invariants = dict()
        # : :type: dict[str, (str, int)]
        self.__achieve_goals = dict()
        # : :type: dict[str, (str, int)]
        self.__achieve_and_sustain_goals = dict()

        # : :type: set[str]
        self.__already_achieved_goals = set()

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

    def arbitrate_verdict(self, toplevel_results, scheduled_results, scheduled_keys):
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

            if PropertyCollection.__check_property_failure(pname, toplevel_results, scheduled_results):
                verdict = NOT_OK
                failed_invariants.add(pname)

        for pname in self.__achieve_goals.keys():
            if pname not in toplevel_results:
                raise SALMAException("Achieve goal {} was not contained in the evaluation "
                                     "step's top-level results ({})!".format(pname, toplevel_results))

            if PropertyCollection.__check_property_success(pname, toplevel_results, scheduled_results):
                self.__already_achieved_goals.add(pname)

        for pname in self.__achieve_and_sustain_goals.keys():
            if pname not in toplevel_results:
                raise SALMAException("Achieve-and-sustain goal {} was not contained in the evaluation "
                                     "step's top-level results ({})!".format(pname, toplevel_results))

            if PropertyCollection.__check_property_success(pname, toplevel_results, scheduled_results):
                self.__already_achieved_goals.add(pname)
            elif (PropertyCollection.__check_property_failure(pname, toplevel_results, scheduled_results)
                  and pname in self.__already_achieved_goals):
                verdict = NOT_OK
                failed_sustain_goals.add(pname)

        for pname in self.properties.keys():
            if pname in scheduled_keys:
                pending_properties.add(pname)

        # if there's no achieve goal, we actually just run until some time limit (see runExperiment)
        if len(self.__achieve_goals) + len(self.__achieve_and_sustain_goals) > 0:
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

    def register_property(self, propertyName, formula, property_type):
        """
        Registers the given formula under the given name.
        :param str propertyName: the name of the property usd for referencing it later.
        :param str formula: the formula.
        :param int property_type: one of World.INVARIANT, World.ACHIEVE, or World.ACHIEVE_AND_SUSTAIN
        """
        if property_type not in (INVARIANT, ACHIEVE,ACHIEVE_AND_SUSTAIN):
            raise SALMAException("Unknown property type: {}".format(property_type))
        if (propertyName in self.__invariants or propertyName in self.__achieve_goals
                or propertyName in self.__achieve_and_sustain_goals):
            raise SALMAException("Property {} already registered.".format(propertyName))
        self.__logics_engine.registerProperty(propertyName, formula)
        if property_type == INVARIANT:
            self.__invariants[propertyName] = (formula, property_type)
        elif property_type == ACHIEVE:
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

    def reset(self):
        self.__already_achieved_goals.clear()