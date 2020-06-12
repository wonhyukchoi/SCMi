import dis

"""
Thanks to Reza Bagheri and his in-depth blog.
https://towardsdatascience.com/understanding-python-bytecode-e7edaae8734d
Check out his other articles too, his explanations are superb.
"""


def unpack_op(bytecode):
    extended_arg = 0
    for i in range(0, len(bytecode), 2):
        opcode = bytecode[i]
        if opcode >= dis.HAVE_ARGUMENT:
            oparg = bytecode[i+1] | extended_arg
            extended_arg = (oparg << 8) if opcode == dis.EXTENDED_ARG else 0
        else:
            oparg = None
        yield i, opcode, oparg


if __name__ == "__main__":
    raise NotImplementedError
