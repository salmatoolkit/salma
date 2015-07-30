from salma.SALMAException import SALMAException
from salma.constants import OK, NOT_OK, NONDET


def verdict_and(verdict1, verdict2):
    if verdict1 is None:
        return verdict2
    elif verdict2 is None:
        return verdict1
    elif verdict1 == OK and verdict2 == OK:
        return OK
    elif verdict1 == NOT_OK or verdict2 == NOT_OK:
        return NOT_OK
    elif verdict1 == NONDET or verdict2 == NONDET:
        return NONDET
    else:
        raise SALMAException("Unsupported verdict values: {} and {} but require one of {}".format(
            verdict1, verdict2, [OK, NOT_OK, NONDET]))


def verdict_or(verdict1, verdict2):
    if verdict1 is None:
        return verdict2
    elif verdict2 is None:
        return verdict1
    elif verdict1 == OK or verdict2 == OK:
        return OK
    elif verdict1 == NOT_OK and verdict2 == NOT_OK:
        return NOT_OK
    elif verdict1 == NONDET or verdict2 == NONDET:
        return NONDET
    else:
        raise SALMAException("Unsupported verdict values: {} and {} but require one of {}".format(
            verdict1, verdict2, [OK, NOT_OK, NONDET]))

