__author__ = 'kroiss'


def choose_alternative(schedule, assignment, blocked=set()):
    """
    Solves an all-different constraint for the variable-domain list in schedule. If successful, the function
    returns True and assignment contains the variable assignment.

    :param list[(str, list)] schedule: the schedule
    :param dict[str, object] assignment: variable assignment
    :param set[str] blocked: a set containing used values for the nested recursion
    :return: bool
    """
    if len(schedule) == 0:
        return True
    current = schedule[0]
    alternatives = current[1]
    for a in alternatives:
        if a not in blocked:
            assignment[current[0]] = a
            success = choose_alternative(schedule[1:], assignment, blocked.union({a}))
            if success:
                return True
    return False

