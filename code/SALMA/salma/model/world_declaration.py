from salma.model.infotransfer import *
from salma.model.actions import Action, StochasticAction
from salma.model.events import ExogenousAction
from salma.model.core import Fluent, Constant


class WorldDeclaration:

    def getFluents(self):
        """
        Returns a list view of all fluents currently registered in the metamodel as a list of core.Fluent objects.
        This list also included constants.
        :rtype: Iterable[Fluent]
        """
        raise NotImplementedError()

    def get_exogenous_action(self, action_name):
        """
        Returns the exogenous action with the given name.
        :raises: SALMAException if there is no exogenous action with the given name.
        :type action_name: str
        :rtype: ExogenousAction
        """
        raise NotImplementedError()

    def getSorts(self):
        """
        Returns a key view of all sort names.
        :rtype: builtins.key_values
        """
        raise NotImplementedError()

    def getAction(self, action_name):
        """
        Returns the action (either deterministic or stochastic) with the given name.
        :raises SALMAException if action is unregistered.
        :type action_name: str
        :rtype: Action
        """
        raise NotImplementedError()

    def get_stochastic_action(self, action_name):
        """
        Returns the stochastic action with the given name.
        :type action_name: str
        :rtype: StochasticAction
        """
        raise NotImplementedError()

    def load_declarations(self):
        """
        Loads the declarations from the domain specification and initializes fluents, constants, and actions.
        """
        raise NotImplementedError()

    def get_exogenous_actions(self):
        """
        Returns a list view on all exogenous actions.
        :rtype: Iterable[ExogenousAction]
        """
        raise NotImplementedError()

    def getAllActions(self):
        """
        Returns a list with all registered deterministic and stochastic actions.
        :rtype: Iterable[Action]
        """
        raise NotImplementedError()

    def get_derived_fluents(self):
        """
        Returns the list of declared derived fluents as tuples of kind (fluent_name, fluent_type, params).
        :rtype: Iterable[(str, str, list)]
        """
        raise NotImplementedError()

    def getConstants(self):
        """
        Returns a list of all registered constants.
        :rtype: Iterable[Constant]
        """
        raise NotImplementedError()

    def getFluent(self, fluentName):
        """
        Returns the core.Fluent object associated by the given fluent name or None if such a fluent
        hasn't been registered.
        :type fluentName: str
        :rtype: Fluent
        """
        raise NotImplementedError()

    def getConstant(self, constantName):
        """
        Returns the core.Constant object associated by the given constant name or None if such a constant
        hasn't been registered.
        :type constantName: str
        :rtype: Constant
        """
        raise NotImplementedError()

    def get_connectors(self):
        """
        Returns the list of registered connectors (channels, sensors and remote sensors).
        :return: the list of connectors
        :rtype: Iterable[Connector]
        """
        raise NotImplementedError()

    def get_connector(self, name: str) -> Connector:
        """
        Returns the connector (channel, sensor, remote sensor) with the given name or None if the name
        is unregistered.
        :param name: the name of the connector.
        :return: the connector or None if no connector with the given name exists
        """
        raise NotImplementedError()

    def get_channels(self):
        """
        Returns the registered channels.
        :rtype: Iterable[Channel]
        """
        raise NotImplementedError()

    def get_sensors(self):
        """
        Returns the registered (local/direct) sensors.
        :rtype: Iterable[Sensor]
        """
        raise NotImplementedError()

    def get_remote_sensors(self):
        """
        Returns the registered remote sensors.
        :rtype: Iterable[RemoteSensor]
        """
        raise NotImplementedError()