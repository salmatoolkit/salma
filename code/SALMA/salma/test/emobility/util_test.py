__author__ = 'kroiss'

import unittest
import salma.test.emobility.utils as utils

class UtilTest(unittest.TestCase):

    def test_choose_alternatives(self):
        schedule = [
            ("a", [1,2,3]),
            ("b", [1,2,3]),
            ("c", [1,3]),
            ("d", [1,2,3,4])
        ]
        assignment = dict()
        success = utils.choose_alternative(schedule, assignment, set())
        print("success: {}".format(success))
        print(str(assignment))



if __name__ == '__main__':
    unittest.main()
