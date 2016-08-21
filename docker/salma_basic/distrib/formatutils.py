from salma.model.data import Term

__author__ = 'Christian'

def format_term(term):
    """
    :rtype: str
    """
    if term is None:
        return "None"
    elif isinstance(term, (tuple, list)):
        processed = []
        for st in term:
            processed.append(format_term(st))
        return "[{}]".format(", ".join(processed))
    elif isinstance(term, Term):
        processed = []
        for p in term.params:
            processed.append(format_term(p))
        return "{}({})".format(term.functor, ", ".join(processed))
    else:
        return str(term)