__author__ = 'christian'


class Bar2(object):

    def __init__(self, val):
        self.__val = val

    @property
    def val(self):
        return self.__val