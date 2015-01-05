import heapq
import unittest
from salma.model.events import ExogenousAction, EventOccurrence


class A:
    def __init__(self, val, name):
        self.__val = val
        self.__name = name

    def say(self):
        print("A(%s): %d" % (self.__name, self.__val))

    def __repr__(self):
        return "A({}, {})".format(self.__val, self.__name)

    def __eq__(self, other):
        return self.__val == other.__val and self.__name == other.__name

    def __lt__(self, other):
        return self.__val < other.__val


class B:
    def __init__(self, val):
        self.__val = val

    def say(self):
        print("B: %d" % self.__val)


class EventOccurrenceTest(unittest.TestCase):

    def test_general_ordering(self):
        a1 = A(1, "a1")
        a2 = A(2, "a2")
        a3 = A(2, "a3")
        l = []
        heapq.heappush(l, a2)
        heapq.heappush(l, a3)
        heapq.heappush(l, a1)
        self.assertFalse(a1 == a2)
        self.assertNotEqual(a1, a2)
        self.assertNotEqual(a2, a3)
        self.assertTrue(a1 < a2)
        self.assertTrue(a2 > a1)
        self.assertFalse(a2 < a3)
        self.assertFalse(a2 > a3)
        self.assertFalse(a2 == a3)
        self.assertListEqual(l, [a1, a3, a2])

    def test_event_occurrence_ordering(self):
        ev1 = ExogenousAction("ev1", [("a", "integer"), ("b", "float")], [])
        ev1b = ExogenousAction("ev1", [("a", "integer"), ("b", "float")], [])
        ev2 = ExogenousAction("ev2", [("a", "integer"), ("b", "float")], [])
        self.assertNotEqual(ev1, ev2)
        self.assertNotEqual(ev1, ev1b)
        occ1 = EventOccurrence(1, ev1, [42, 3.14])
        occ2 = EventOccurrence(2, ev1, [42, 3.14])
        occ3 = EventOccurrence(2, ev2, [42, 3.14])
        self.assertTrue(occ1 < occ2)
        self.assertFalse(occ2 < occ3)
        l = []
        heapq.heappush(l, occ3)
        heapq.heappush(l, occ2)
        heapq.heappush(l, occ1)
        print(l)
        self.assertEqual(l[0], occ1)
        occ4 = EventOccurrence(2, ev2, [42, 3.14])
        self.assertTrue(occ4 in l)



