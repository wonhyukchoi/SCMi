import unittest
import random
from linked_list import LinkedList


class TestDS(unittest.TestCase):
    def test_list(self, limit=10000):
        random_list = [random.randint(0, limit) for _ in range(limit)]
        llist = LinkedList(random_list)
        for i in range(limit):
            self.assertEqual(random_list[i], llist[i])


if __name__ == "__main__":
    unittest.main()
