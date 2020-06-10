# noinspection PyPep8Naming
class PVM:
    """
    Python virtual machine
    -- bytecode interpreter --
    written in Python.
    """

    def __init__(self):
        self.stack = []
        self.environment = {}

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

    def STORE_NAME(self, name):
        value = self.stack.pop()
        self.environment[name] = value

    def LOAD_NAME(self, name):
        value = self.environment[name]
        self.stack.append(value)

    @staticmethod
    def parse_args(opname, oparg, bytecode):
        if opname in ('LOAD_VALUE',):
            value = bytecode['numbers'][oparg]
        elif opname in ('STORE_NAME', 'LOAD_NAME'):
            value = bytecode['names'][oparg]
        else:
            value = None

        return value

    def execute(self, bytecode):
        instruction_list = bytecode['instructions']

        for (opname, oparg) in instruction_list:
            argument = self.parse_args(opname, oparg, bytecode)

            if opname == 'LOAD_VALUE':
                self.LOAD_VALUE(number=argument)
            elif opname == 'ADD_TWO_VALUES':
                self.ADD_TWO_VALUES()
            elif opname == 'PRINT_ANSWER':
                self.PRINT_ANSWER()
            elif opname == 'STORE_NAME':
                self.STORE_NAME(name=argument)
            elif opname == 'LOAD_NAME':
                self.LOAD_NAME(name=argument)
            else:
                raise KeyError


if __name__ == "__main__":
    what_to_execute = {
        "instructions": [("LOAD_VALUE", 0),
                         ("STORE_NAME", 0),
                         ("LOAD_VALUE", 1),
                         ("STORE_NAME", 1),
                         ("LOAD_NAME", 0),
                         ("LOAD_NAME", 1),
                         ("ADD_TWO_VALUES", None),
                         ("PRINT_ANSWER", None)],
        "numbers": [1, 2],
        "names":   ["a", "b"]}
    pvm = PVM()
    pvm.execute(what_to_execute)
