import collections.abc


def min_robust(values):
    """
    Returns the smallest of the iterable values. The iterable may also contain None entries that are ignored.
    :param collections.abc.Iterable values: the values to compare
    :rtype: object
    """
    m = None
    assert isinstance(values, collections.abc.Iterable)
    for v in values:
        if m is None:
            m = v
        else:
            if v is not None and v < m:
                m = v
    return m

def max_robust(values):
    """
    Returns the highest of the iterable values. The iterable may also contain None entries that are ignored.
    :param collections.abc.Iterable values: the values to compare
    :rtype: object
    """
    m = None
    assert isinstance(values, collections.abc.Iterable)
    for v in values:
        if m is None:
            m = v
        else:
            if v is not None and v > m:
                m = v
    return m


def max_robust(a, b):
    """
    Returns the highest of a or b. Both a or b could be None, in which case the other value is returned.
    :param object a: the first compared object
    :param object b: the second compared object
    :rtype: object
    """
    return max_robust([a, b])

