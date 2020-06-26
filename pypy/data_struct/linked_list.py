class LinkedList:
    def __init__(self, python_list=None):
        self.head = None
        self._len = 0

        if python_list is not None:
            self._build_from_py_list(python_list)

    def append(self, value):
        if self.head is None:
            self.head = LinkedListNode(value=value)
            self._len += 1
        else:
            node = self._traverse(self._len - 1)
            new = LinkedListNode(value)
            node.pointer = new
            self._len += 1

    def _traverse(self, index):
        traversing = self.head
        for _ in range(index):
            traversing = traversing.pointer
            if traversing is None:
                raise IndexError(f"{index} exceeds length of list")
        return traversing

    def _build_from_py_list(self, py_list: list):
        if type(py_list) is not list:
            raise TypeError(f"Expected list, but got {type(py_list)}")

        self._len = len(py_list)

        self.head = LinkedListNode(py_list[0])
        traversing = self.head
        for elem in py_list[1:]:
            new = LinkedListNode(value=elem)
            traversing.pointer = new
            traversing = new

    def __getitem__(self, item):
        node = self._traverse(item)
        return node.value

    def __setitem__(self, key, value):
        node = self._traverse(key)
        node.value = value

    def __len__(self):
        return self._len

    def __repr__(self):
        traversing = self.head
        s = ""
        for i in range(self._len):
            s += f"Elem {i}: {traversing.value}\n"
            traversing = traversing.pointer
        s = s[:-1]  # To remove last newline
        return s

    def __iter__(self):
        raise NotImplementedError


class LinkedListNode:
    def __init__(self, value, pointer=None):
        self.value = value
        self.pointer = pointer
