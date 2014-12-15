import random
from salma.SALMAException import SALMAException
from salma.model.evaluationcontext import EvaluationContext

class OutcomeSelectionStrategy:
    def __init__(self):
        #: :type: dict[str, object]
        self.__options = dict()

    @property
    def options(self):
        """
        The options available to this selection strategy.
        :rtype: dict[str, object]
        """
        return self.__options

    @options.setter
    def options(self, value):
        self.__options = value

    def select_outcome(self, evaluation_context, param_values):
        """
        Selects a RandomActionOutcome.
        :param EvaluationContext evaluation_context: the evaluation context
        :param list param_values: the parameter values

        :return: the outcome
        :rtype: object
        """
        raise NotImplementedError()

    def check(self, action_dict):
        """
        Checks if the configuration is consistent.
        :return: None if consistent or a list of problem description

        :type action_dict: dict of (str, Action)
        :rtype: list of tuple
        """
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

    def set_probability(self, action_name, probability):
        """
        Sets the selection probability for the given action.
        :type action_name: str
        :type probability: float
        """
        self.__probabilities[action_name] = probability

    def probability(self, action_name: str) -> float:
        """
        Returns the selection probability for the given action.
        """
        try:
            return self.__probabilities[action_name]
        except KeyError:
            raise SALMAException("No probability specified for action {}.".format(action_name))

    def select_outcome(self, evaluation_context, param_values):
        r = random.uniform(0, 1)
        start = 0
        for action, probability in self.__probabilities.items():
            if (r >= start) and (r < start + probability):
                return self.options[action]
            start += probability
        raise (
            SALMAException("No outcome could be selected, r = {}, probabilities = {}".format(r, self.__probabilities)))

    def check(self, action_dict):
        """
        :type action_dict: dict of (str, Action)
        :rtype: list of tuple
        """
        problems = super().check(action_dict)
        for outcome in self.action.outcomes:
            if outcome.action_name not in self.__probabilities:
                problems.append(("outcome_selection_strategy.stepwise.no_prob_for_outcome", outcome.action_name))

        s = sum(self.__probabilities.values())
        if s != 1.0:
            problems.append(("outcome_selection_strategy.stepwise.wrong_prob_sum", s))

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
        return random.choice(list(self.options.values()))

    def describe(self):
        return "Uniform"


class Deterministic(OutcomeSelectionStrategy):
    """
    Creates a trivial deterministic stochastic action outcome selector that is parametric in the sense that there
    is only one outcome specification.
    """

    def __init__(self):
        super().__init__()

    def select_outcome(self, evaluation_context, param_values):
        return list(self.options.values())[0]

    def check(self, action_dict):
        problems = super().check(action_dict)
        if len(self.options) != 1:
            problems.append(("outcome_selection_strategy.deterministic.more_than_one_outcome", None))
        return problems

    def describe(self):
        return "Deterministic({})".format(self.action.outcomes[0].describe())