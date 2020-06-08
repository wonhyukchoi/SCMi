# noinspection PyPep8Naming
class PVM:
    """
    Python virtual machine
    -- bytecode interpreter --
    written in Python.
    """
    def __init__(self):
        self.stack = []

    def LOAD_VALUE(self, number):
        self.stack.append(number)

    def PRINT_ANSWER(self):
        answer = self.stack.pop()
        print(answer)

    def ADD_TWO_VALUES(self):
        fst_val = self.stack.pop()
        snd_val = self.stack.pop()
        sum_val = fst_val + snd_val
        self.stack.append(sum_val)
