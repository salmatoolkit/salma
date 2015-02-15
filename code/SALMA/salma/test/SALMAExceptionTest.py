__author__ = 'Christian'

import unittest
from salma.SALMAException import SALMAException


def raise_salma_exception(msg, **kwargs):
    print("raise_salma_exception")
    if "sit" in kwargs:
        print(kwargs["sit"])
    raise SALMAException(msg)


class ExceptionTest(unittest.TestCase):

    def test_raise_exception(self):
        try:
            raise_salma_exception("test", sit=42)
        except Exception as exp:
            print(type(exp))
            self.assertTrue(isinstance(exp, SALMAException))


if __name__ == '__main__':
    unittest.main()
