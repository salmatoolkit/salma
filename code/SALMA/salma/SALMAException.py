

class SALMAException(Exception):

    def __init__(self, msg):
        """
        Creates an exception to be used in the SALMA context.
        :param str msg: the message
        """
        super().__init__()
        self.__message = msg

    @property
    def message(self):
        return self.__message

    def __str__(self):
        return "{}".format(self.__message)



