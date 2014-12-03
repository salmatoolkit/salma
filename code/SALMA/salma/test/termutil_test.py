import unittest
from salma.termutils import tuplify
from salma.model.core import Term
from collections.abc import Hashable


class TermUtilTest(unittest.TestCase):

    def test_tuplify(self):
        self.assertEqual(None, tuplify(None))
        self.assertEqual(42, tuplify(42))
        self.assertEqual("hello", tuplify("hello"))
        self.assertEqual((), tuplify([]))
        self.assertEqual((), tuplify(()))
        self.assertEqual((42,), tuplify((42,)))
        self.assertEqual((42,), tuplify([42]))
        self.assertEqual((42, 4711, "test"), tuplify([42, 4711, "test"]))
        self.assertEqual((42, 4711, ("test1", 0, "test2")), tuplify([42, 4711, ["test1", 0, "test2"]]))

    def term_test(self):
        t = Term("foo", 1, 0, ["a", 42])
        t2 = Term("foo", 1, 0, ["a", 42])
        self.assertEqual(str(t), "foo(1, 0, ('a', 42))")
        self.assertEqual(t, t2)
        self.assertTrue(t == t2)
        self.assertTrue(isinstance(t, Hashable))
        myset = set()
        myset.add(t)
        myset.add(t2)
        self.assertEqual(len(myset), 1)
        self.assertTrue(t in myset)
        self.assertTrue(t2 in myset)
        d = dict()
        d[t] = 42
        self.assertEqual(d[t2], 42)


if __name__ == '__main__':
    unittest.main()
