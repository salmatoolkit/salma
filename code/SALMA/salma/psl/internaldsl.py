import re

FREEVAR_PATTERN = re.compile(r"\{(.*?)\}")

class PSLTerm:
    def __init__(self, functor, subterms, infix=False):
        """
        :param str|PSLTerm functor: the functor
        :param list[PSLTerm] subterms: the subterms
        """
        self.__functor = None if functor is None else makepslterm(functor)
        self.__subterms = subterms
        self.__infix = infix

    @property
    def functor(self):
        return self.__functor

    @property
    def subterms(self):
        """
        :rtype: list[PSLTerm]
        """
        return self.__subterms

    @property
    def infix(self) -> bool:
        return self.__infix

    def __str__(self, *args, **kwargs):
        if self.infix:
            if len(self.subterms) != 2:
                raise ValueError("Expected 2 subterms for infix operator {} but got {}.".format(self.functor),
                                 len(self.subterms))
            return "{} {} {}".format(str(self.subterms[0]), str(self.functor), str(self.subterms[1]))
        else:
            if len(self.subterms) == 0:
                return self.functor
            else:
                return "{}({})".format(str(self.functor), ", ".join(map(str, self.subterms)))

    def __repr__(self, *args, **kwargs):
        return "{}({})".format(type(self).__name__, ", ".join(map(repr, self.subterms)))

    def get_free_variables(self):
        """
        :rtype: list[str]
        """
        fvars = []
        if isinstance(self.__functor, FreeVariable):
            fvars.append(self.__functor.name)
        for st in self.subterms:
            if isinstance(st, FreeVariable):
                fvars.append(st.name)
            else:
                fvars.extend(st.get_free_variables())
        return fvars


class Forall(PSLTerm):
    def __init__(self, varname, sort, body):
        """
        :param str varname: the variable name
        :param str sort: the sort
        :param body: the body
        """
        self.__body = makepslterm(body)
        self.__varname = makepslterm(varname)
        self.__sort = makepslterm(sort)
        super().__init__("forall", [self.__varname, self.__sort, self.__body])

    @property
    def varname(self) -> PSLTerm:
        return self.__varname

    @property
    def sort(self) -> PSLTerm:
        return self.__sort

    @property
    def body(self) -> PSLTerm:
        return self.__body

    def __str__(self, *args, **kwargs):
        return "forall({}:{}, {})".format(str(self.varname), str(self.sort), str(self.body))


class Exists(PSLTerm):
    def __init__(self, varname, sort, body):
        """
        :param str varname: the variable name
        :param str sort: the sort
        :param body: the body
        """
        self.__body = makepslterm(body)
        self.__varname = makepslterm(varname)
        self.__sort = makepslterm(sort)
        super().__init__("exists", [self.__varname, self.__sort, self.__body])

    @property
    def varname(self) -> PSLTerm:
        return self.__varname

    @property
    def sort(self) -> PSLTerm:
        return self.__sort

    @property
    def body(self) -> PSLTerm:
        return self.__body

    def __str__(self, *args, **kwargs):
        return "exists({}:{}, {})".format(str(self.varname), str(self.sort), str(self.body))

    def __repr__(self, *args, **kwargs):
        return "Exists({}, {}, {})".format(self.varname, self.sort, repr(self.body))


class Implies(PSLTerm):
    def __init__(self, premise, consequence):
        self.__premise = makepslterm(premise)
        self.__consequence = makepslterm(consequence)
        super().__init__("implies", [self.__premise, self.__consequence])

    @property
    def premise(self):
        return self.__premise

    @property
    def consequence(self):
        return self.__consequence


class And(PSLTerm):
    def __init__(self, *subterms):
        super().__init__("and", list(map(makepslterm, subterms)))


class Or(PSLTerm):
    def __init__(self, *subterms):
        super().__init__("or", list(map(makepslterm, subterms)))


class Not(PSLTerm):
    def __init__(self, body):
        super().__init__("not", [makepslterm(body)])


class Let(PSLTerm):
    def __init__(self, varname, definition, body):
        self.__varname = makepslterm(varname)
        self.__definition = makepslterm(definition)
        self.__body = makepslterm(body)
        super().__init__("let", [self.__varname, self.__definition, self.__body])

    @property
    def varname(self) -> PSLTerm:
        return self.__varname

    @property
    def definition(self) -> PSLTerm:
        return self.__definition

    @property
    def body(self) -> PSLTerm:
        return self.__body

    def __str__(self, *args, **kwargs):
        return "let({}: {}, {})".format(str(self.varname), str(self.definition), str(self.body))


class Eventually(PSLTerm):
    def __init__(self, time_limit: int, body):
        self.__body = makepslterm(body)
        self.__time_limit = makepslterm(time_limit)
        super().__init__("eventually", [self.__time_limit, self.__body])

    @property
    def time_limit(self) -> PSLTerm:
        return self.__time_limit

    @property
    def body(self) -> PSLTerm:
        return self.__body

    def __str__(self, *args, **kwargs):
        return "eventually({}, {})".format(str(self.time_limit), str(self.body))


class Always(PSLTerm):
    def __init__(self, time_limit, body):
        self.__body = makepslterm(body)
        self.__time_limit = makepslterm(time_limit)
        super().__init__("always", [self.__time_limit, self.__body])

    @property
    def time_limit(self):
        return self.__time_limit

    @property
    def body(self):
        return self.__body

    def __str__(self, *args, **kwargs):
        return "always({}, {})".format(self.time_limit, str(self.body))


class Until(PSLTerm):
    def __init__(self, time_limit: int, p, q):
        self.__p = makepslterm(p)
        self.__q = makepslterm(q)
        self.__time_limit = makepslterm(time_limit)
        super().__init__("until", [self.__time_limit, self.__p, self.__q])

    @property
    def time_limit(self) -> PSLTerm:
        return self.__time_limit

    @property
    def p(self) -> PSLTerm:
        return self.__p

    @property
    def q(self) -> PSLTerm:
        return self.__q

    def __str__(self, *args, **kwargs):
        return "until({}, {}, {})".format(str(self.time_limit), str(self.p), str(self.q))


class ActionInstance(PSLTerm):
    def __init__(self, action, arguments):
        self.__action = makepslterm(action)
        self.__arguments = list(map(makepslterm, arguments))
        super().__init__(self.__action, self.__arguments)

    @property
    def action(self) -> PSLTerm:
        return self.__action

    @property
    def arguments(self):
        """
        :rtype: list[PSLTerm]
        """
        return self.__arguments


class Occur(PSLTerm):
    def __init__(self, action, arguments):
        self.__action = ActionInstance(action, arguments)
        super().__init__("occur", [self.__action])

    @property
    def action(self) -> ActionInstance:
        return self.__action

    def __str__(self, *args, **kwargs):
        return "occur({})".format(str(self.__action))


class NoneTerm(PSLTerm):
    def __init__(self):
        super().__init__("none", [])

    def __str__(self, *args, **kwargs):
        return "none"

    def __repr__(self, *args, **kwargs):
        return "None"


class LiteralExpression(PSLTerm):
    def __init__(self, value):
        super().__init__(None, [])
        self.__value = value

    @property
    def value(self):
        return self.__value

    def __str__(self, *args, **kwargs):
        if self.value is None:
            return "none"
        elif isinstance(self.value, bool):
            return "true" if self.value == True else "false"
        else:
            return str(self.value)

    def __repr__(self, *args, **kwargs):
        return "LiteralExpression({})".format(repr(self.value))

    def get_free_variables(self):
        if self.value is None:
            return []
        sval = str(self.value)
        return FREEVAR_PATTERN.findall(sval)


class FreeVariable(PSLTerm):
    def __init__(self, name):
        super().__init__(None, [])
        self.__name = name

    @property
    def name(self):
        return self.__name

    def __str__(self, *args, **kwargs):
        return "{{{}}}".format(self.name)

    def __repr__(self, *args, **kwargs):
        return "FreeVariable({})".format(self.name)


def makepslterm(val) -> PSLTerm:
    if val is None:
        return LiteralExpression(None)
    elif isinstance(val, PSLTerm):
        return val
    elif isinstance(val, str):
        m = FREEVAR_PATTERN.match(val)
        if m is not None:
            return FreeVariable(m.group(1))
        else:
            return LiteralExpression(val)
    else:
        return LiteralExpression(val)
