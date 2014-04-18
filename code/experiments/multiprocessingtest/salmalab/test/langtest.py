from salmalab.test import Bar2


class Foo(object):
    def __init__(self, factor):
        self.__factor = factor
    def __call__(self, a=None, b=None, *args, **kwargs):
        print(a * self.__factor, b)

    def describe(self):
        print("Foo : %d" % self.__factor)

    def __getitem__(self, item):
        return "Item %s " % str(item)


def bar(f: Foo=None):
    f(4, 5)
    f.describe()

if __name__ == '__main__':
    myf = Foo(3)
    bar(myf)
    bar2 = Bar2.Bar2(42)
    print(bar2.val)