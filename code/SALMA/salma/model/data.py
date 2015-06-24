from salma.termutils import tuplify

__author__ = 'kroiss'


class Term(object):
    """
    Represents terms used in actions or as content of fluents.
    """

    def __init__(self, functor: str, *params):
        self.__functor = functor
        #: :type: tuple
        self.__params = tuplify(params)

    @property
    def arity(self) -> int:
        return len(self.__params)

    @property
    def functor(self) -> str:
        return self.__functor

    @property
    def params(self) -> tuple:
        return self.__params

    def __hash__(self, *args, **kwargs):
        return hash((self.__functor, self.__params))

    def __eq__(self, *args, **kwargs):
        return (self.__functor, self.__params) == args[0]

    def __repr__(self, *args, **kwargs):
        return "Term(\"{}\", {})".format(self.__functor, self.__params)

    def __str__(self, *args, **kwargs):
        return "{}({})".format(self.__functor, ", ".join(map(str, self.__params)))
