from typing import List, Undefined


class A:
    def __init__(self, v):
        self.__val = v

    @property
    def val(self):
        return self.__val


def foo(x: A) -> int:
    return x.val


def bar(vals: List[A]):
    s = Undefined(str)
    print(s)
    for v in vals:
        print(v.val)

if __name__ == '__main__':
    a = A(42)
    print(foo(a))
    a2 = A(12)
    bar([a, a2])