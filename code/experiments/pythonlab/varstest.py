__author__ = 'Christian'

my_dict = {'a': 1, 'b': 2}


def foo(v, l=[]):
    l.append(v)
    print(l)


def bar(v: int):
    if v > 0:
        res = [1]
    else:
        res = [-1]

    return res


def check_dict(some_dict: dict[str, int], key: str) -> int:
    return some_dict[key]

if __name__ == '__main__':
    foo(1)
    foo(2)