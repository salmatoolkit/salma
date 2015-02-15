class A:

    def __init__(self, num):
        self.__num = num

    def perform(self):
        print("A")


    def get_num(self):
        return self.__num


class B:
    def perform(self):
        print("B")


class AB(B, A):

    def __init__(self):
        A.__init__(self, 42)

    def ping(self):
        print(super())

    def perform(self):
        print("C")
        super().perform()


if __name__ == '__main__':
    ab = AB()
    ab.ping()
    print(ab.get_num())
    ab.perform()



