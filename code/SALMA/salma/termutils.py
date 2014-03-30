__author__ = 'kroiss'


def term_from_tuple(tupleterm):
    """
    :param tupleterm: The tuple-representation of the term.
    :rtype: str
    """
    if tupleterm is None:
        return "None"
    elif isinstance(tupleterm, tuple):
        if len(tupleterm) == 0:
            return "None"
        functor = tupleterm[0]
        params = []
        for p in tupleterm[1:]:
            params.append(term_from_tuple(p))
        return "{}({})".format(functor, ", ".join(params))
    elif isinstance(tupleterm, list):
        params = []
        for p in tupleterm:
            params.append(term_from_tuple(p))
        return "[{}]".format(", ".join(params))
    else:
        return str(tupleterm)




