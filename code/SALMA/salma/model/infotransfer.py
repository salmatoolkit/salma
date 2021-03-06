from salma.SALMAException import SALMAException
from salma.model.data import Term


class Connector(object):
    def __init__(self, name: str):
        self.__name = name

    @property
    def name(self) -> str:
        return self.__name


class Channel(Connector):
    UNICAST = 1
    MULTICAST = 2

    def __init__(self, name: str, role1: tuple, role2: tuple, mode):
        """
        Creates a channel with the given roles and type.
        :param mode: UNICAST|MULTICAST
        :type mode: str|int
        """
        super().__init__(name)
        if (isinstance(role1, (tuple, list)) and
                len(role1) == 2 and
                isinstance(role1[0], str) and
                isinstance(role1[1], str)):
            self.__role1 = role1
        else:
            raise SALMAException(
                "Wrong format for role 1 of channel {}: {} but expected (name, type)".format(self.name, role1))

        if (isinstance(role2, (tuple, list)) and
                len(role2) == 2 and
                isinstance(role2[0], str) and
                isinstance(role2[1], str)):
            self.__role2 = role2
        else:
            raise SALMAException(
                "Wrong format for role 2 of channel {}: {} but expected (name, type)".format(self.name, role2))

        self.__mode = -1
        if mode in (Channel.UNICAST, Channel.MULTICAST):
            self.__mode = mode
        elif isinstance(mode, str):
            m2 = mode.strip().lower()
            if m2 == "unicast":
                self.__mode = Channel.UNICAST
            elif m2 == "multicast":
                self.__mode = Channel.MULTICAST
        if self.__mode < 0:
            raise SALMAException("Unsupported channel mode: {}".format(mode))

    @property
    def role1(self):
        """
        Returns the first role of the channel as a (name, type) tuple.
        :rtype: (str, str)
        """
        return self.__role1

    @property
    def role2(self):
        """
        Returns the second role of the channel as a (name, type) tuple.
        :rtype: (str, str)
        """
        return self.__role2

    @property
    def mode(self) -> int:
        return self.__mode


class Sensor(Connector):
    def __init__(self, name: str, owner_type: str, source_fluent: str):
        super().__init__(name)
        self.__owner_type = owner_type
        self.__source_fluent = source_fluent

    @property
    def owner_type(self) -> str:
        return self.__owner_type

    @property
    def source_fluent(self) -> str:
        return self.__source_fluent


class RemoteSensor(Connector):
    def __init__(self, name: str, remote_sensor_owner_type: str, local_sensor_name: str, local_sensor_owner_type):
        super().__init__(name)
        self.__remote_sensor_owner_type = remote_sensor_owner_type
        self.__local_sensor_name = local_sensor_name
        self.__local_sensor_owner_type = local_sensor_owner_type

    @property
    def remote_sensor_owner_type(self) -> str:
        return self.__remote_sensor_owner_type

    @property
    def local_sensor_name(self):
        return self.__local_sensor_name

    @property
    def local_sensor_owner_type(self):
        return self.__local_sensor_owner_type


class ReceivedMessage(object):
    """
    A class for message objects in a channel's input queue. These objects are generated by procedure.Receive.
    """

    def __init__(self, message_term):
        """
        :type message_term: Term

        format: msg(Sender, SrcRole, Dest, DestRole, Time, Content2)
        """
        (self.__sender, self.__src_role, self.__dest, self.__dest_role,
         self.__time, self.__content) = message_term.params

    @property
    def sender(self):
        return self.__sender

    @property
    def src_role(self):
        return self.__src_role

    @property
    def destination(self):
        return self.__dest

    @property
    def dest_role(self):
        return self.__dest_role

    @property
    def content(self):
        return self.__content



