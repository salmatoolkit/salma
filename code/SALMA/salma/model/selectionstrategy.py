import random
from salma.SALMAException import SALMAException
from salma.model.evaluationcontext import EvaluationContext


class OutcomeSelectionStrategy:
    def __init__(self):
        #: :type: ()->dict[str, object]
        self.__option_provider = None

    @property
    def options(self):
        """
        The options available to this selection strategy.
        :rtype: dict[str, object]
        """
        if self.__option_provider is None:
            raise SALMAException("No option provider defined.")
        return self.__option_provider()

    def set_option_provider(self, option_provider):
        """
        Sets the option provider, a function that returns a dict of form (outcome_name -> outcome).
        :param (()->dict[str, object])|None option_provider: the option provider function.
        """
        self.__option_provider = option_provider

    def select_outcome(self, evaluation_context, param_values):
        """
        Selects a RandomActionOutcome.
        :param EvaluationContext evaluation_context: the evaluation context
        :param list param_values: the parameter values

        :return: the outcome
        :rtype: object
        """
        raise NotImplementedError()

    def check(self):
        """
        Checks if the configuration is consistent.
        :return: Empty list if consistent or a list of problem description of form (problem_key, params)
        :rtype: list[(str, tuple)]
        """
        if self.__option_provider is None:
            return [("no_option_provider_defined", ())]
        return []

    def describe(self):
        """
        Returns a textual representation of the outcome selection strategy.
        :rtype: str
        """
        return "n/a"


class Stepwise(OutcomeSelectionStrategy):
    """
    Creates a selection strategy where a selection probability is given for each outcome.
    """

    def __init__(self, *args, **kwargs):
        """
        :param list[(str, float)] probabilities: a list of (action_name, probability)
        """
        super().__init__()
        # : :type: dict of (str, float)
        self.__probabilities = dict()
        # alternative 1: one list of tuples
        if len(args) == 1 and isinstance(args[0], list):
            for p in args[0]:
                self.__probabilities[p[0]] = p[1]
        else:
            for outcome, prob in kwargs.items():
                self.__probabilities[outcome] = prob

    @property
    def probabilities(self):
        """
        The selection properties for all action outcome.
        :rtype: dict[str, float]
        """
        return self.__probabilities

    def set_probability(self, option_name, probability):
        """
        Sets the selection probability for the given action.
        :type option_name: str
        :type probability: float
        """
        self.__probabilities[option_name] = probability

    def probability(self, option_name: str) -> float:
        """
        Returns the selection probability for the given action.
        """
        try:
            return self.__probabilities[option_name]
        except KeyError:
            raise SALMAException("No probability specified for option {}.".format(option_name))

    def select_outcome(self, evaluation_context, param_values):
        r = random.uniform(0, 1)
        start = 0
        for action, probability in self.__probabilities.items():
            if (r >= start) and (r < start + probability):
                return self.options[action]
            start += probability
        raise (
            SALMAException("No outcome could be selected, r = {}, probabilities = {}".format(r, self.__probabilities)))

    def check(self):
        """
        :rtype: list of tuple
        """
        problems = super().check()
        for option in self.options.keys():
            if option not in self.probabilities:
                problems.append(("outcome_selection_strategy.stepwise.no_prob_for_outcome", (option, )))

        s = sum(self.__probabilities.values())
        if s != 1.0:
            problems.append(("outcome_selection_strategy.stepwise.wrong_prob_sum", (s, )))
        return problems

    def describe(self):
        probs = []
        for action, prob in self.probabilities.items():
            probs.append("{}:{:.4}".format(action, prob))
        return "Stepwise({})".format(", ".join(probs))


class Uniform(OutcomeSelectionStrategy):
    """
    Creates an outcome selection strategy that chooses one outcome uniformly from the list of outcomes.
    """

    def __init__(self):
        super().__init__()

    def select_outcome(self, evaluation_context, param_values):
        l = list(self.options.values())
        if len(l) == 0:
            raise SALMAException("No outcome specified!")
        elif len(l) == 1:
            return l[0]
        else:
            return random.choice(l)

    def describe(self):
        return "Uniform"


class Deterministic(OutcomeSelectionStrategy):
    """
    Creates a trivial deterministic action outcome selector that always chooses the same outcome.
    """

    def __init__(self, selected_outcome):
        """
        :param str selected_outcome: the outcome that will be every time.
        """
        self.__selected_outcome = selected_outcome
        super().__init__()

    @property
    def selected_outcome(self):
        return self.__selected_outcome

    def select_outcome(self, evaluation_context, param_values):
        try:
            outcome = self.options[self.selected_outcome]
            return outcome
        except KeyError:
            raise SALMAException("Option {} not defined!".format(self.selected_outcome))

    def check(self):
        problems = super().check()
        if self.selected_outcome not in self.options:
            problems.append(("option_not_defined", (self.selected_outcome,)))

        return problems

    def describe(self):
        return "Deterministic({})".format(self.selected_outcome)