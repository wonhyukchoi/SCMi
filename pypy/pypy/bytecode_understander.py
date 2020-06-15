import dis
from types import CodeType
from pprint import pprint

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


def find_linestarts(code_obj: CodeType):
    byte_increments = code_obj.co_lnotab[0::2]
    line_increments = code_obj.co_lnotab[1::2]
    byte = 0
    line = code_obj.co_firstlineno
    linestart_dict = {byte: line}
    for byte_incr, line_incr in zip(byte_increments,
                                    line_increments):
        byte += byte_incr

        if line_incr >= 0x80:
            line_incr -= 0x100
        line += line_incr

        linestart_dict[byte] = line

    return linestart_dict


def factorial(n):
    result = n
    for i in range(1, n):
        result *= i
    return result


"""Below this line is copypasta."""


def get_argvalue(offset, codeobj, opcode, oparg):
    constants= codeobj.co_consts
    varnames = codeobj.co_varnames
    names = codeobj.co_names
    cell_names = codeobj.co_cellvars + codeobj.co_freevars
    argval = None
    if opcode in dis.hasconst:
        if constants is not None:
            argval = constants[oparg]
            if type(argval)==str or argval==None:
                 argval = repr(argval)
    elif opcode in dis.hasname:
        if names is not None:
            argval = names[oparg]
    elif opcode in dis.hasjrel:
        argval = offset + 2 + oparg
        argval = "to " + repr(argval)
    elif opcode in dis.haslocal:
        if varnames is not None:
            argval = varnames[oparg]
    elif opcode in dis.hascompare:
        argval = dis.cmp_op[oparg]
    elif opcode in dis.hasfree:
        if cell_names is not None:
            argval = cell_names[oparg]
    return argval


def findlabels(codeobj):
    bytecode = codeobj.co_code
    labels = []
    for offset, opcode, oparg in unpack_op(bytecode):
            if opcode in dis.hasjrel:
                label = offset + 2 + oparg
            elif opcode in dis.hasjabs:
                label = oparg
            else:
                continue
            if label not in labels:
                labels.append(label)
    return labels


def disassemble(c):
    if not(hasattr(c, 'co_code')):
        raise TypeError("The argument should be a code object")
    code_objects = []
    linestarts = find_linestarts(c)
    labels = findlabels(c)
    bytecode = c.co_code
    extended_arg = 0
    for offset, opcode, oparg in unpack_op(bytecode):
        argvalue = get_argvalue(offset, c, opcode, oparg)
        if hasattr(argvalue, 'co_code'):
            code_objects.append(argvalue)
        line_start = linestarts.get(offset, None)
        dis_text =  "{0:4}{1:2}{2:5} {3:<22} {4:3} {5}".format(str(line_start or ''),
                                                        ">>" if offset in labels else "",
                                                        offset, dis.opname[opcode],
                                                        oparg if oparg is not None else '',
                                                        "(" + str(argvalue) + ")" if argvalue is not
                                                                                        None else '')
        if (line_start and offset):
            print()
        print(dis_text)
    for oc in code_objects:
        print("\nDisassembly of{}:\n".format(oc))
        disassemble(oc)


def disassemble_to_list(c):
    code_list = []
    bytecode = c.co_code
    for offset, opcode, oparg in unpack_op(bytecode):
        argval = get_argvalue(offset, c, opcode, oparg)
        if argval is not None:
            if type(argval)==str:
                argval = argval.strip("\'")
            argval = None if argval=='None' else argval
            code_list.append([dis.opname[opcode], argval])
        else:
            if oparg is not None:
                code_list.append([dis.opname[opcode], oparg])
            else:
                code_list.append([dis.opname[opcode]])
    return code_list



def get_oparg(offset, opcode, argval, constants, varnames, names, cell_names):
    oparg = argval
    if opcode in dis.hasconst:
        if constants is not None:
            oparg = constants.index(argval)
    elif opcode in dis.hasname:
        if names is not None:
            oparg = names.index(argval)
    elif opcode in dis.hasjrel:
        argval = int(argval.split()[1])
        oparg = argval - offset - 2
    elif opcode in dis.haslocal:
        if varnames is not None:
            oparg = varnames.index(argval)
    elif opcode in dis.hascompare:
        oparg = dis.cmp_op.index(argval)
    elif opcode in dis.hasfree:
        if cell_names is not None:
            oparg = cell_names.index(argval)
    return oparg


def assemble(code_list, constants, varnames, names, cell_names):
    byte_list = []
    for i, instruction in enumerate(code_list):
        if len(instruction)==2:
            opname, argval = instruction
            opcode = dis.opname.index(opname)
            oparg = get_oparg(i*2, opcode, argval, constants, varnames, names, cell_names)
        else:
            opname = instruction[0]
            opcode = dis.opname.index(opname)
            oparg = 0
        byte_list += [opcode, oparg]
    return(bytes(byte_list))


class ConstError(Exception):
    pass

def add_const(cl):
    '''Detects the declared constants and modifies the disassembled bytecode list after that. 
    Raises an exception if that constat is reassigned.
    
    Parameters
    =========================================
    cl (list): disassembled bytecode list
    
    Returns
    =========================================
    tuple: a tuple of the modified bytecode list plus the tuple of constant variables
    '''
    code_list = cl.copy()
    constants= []
    indices = []
    
    # Find the variables declared as const. Add their name and index to constants and indices list
    for index, instruction in enumerate(code_list[:-1]):
            if instruction == ['LOAD_GLOBAL', 'const']:
                code_list[index]=['NOP']
                next_instruction = code_list[index+1] 
                if (next_instruction[0]=='STORE_ATTR'):
                    if next_instruction[1] in constants:
                        raise ConstError("You cannot declare a constant variable twice!")
                    else:
                        constants.append(next_instruction[1])
                        indices.append(index+1)
                        code_list[index+1][0]='STORE_FAST'   
                else:
                    raise ConstError("The constant variable should be assigned after declaration!")
    
    #If a constant variable has been reassigned then raise an exception
    for index, instruction in enumerate(code_list[:-1]):
            if (instruction[0] == 'LOAD_GLOBAL') and (instruction[1] in constants):
                code_list[index][0] = 'LOAD_FAST'
            if (instruction[0] == 'STORE_GLOBAL' or instruction[0] == 'STORE_FAST') and \
            (instruction[1] in constants) and index not in indices:
                raise ConstError("'"+instruction[1]+"' is a constant and cannot be reassigned!")               
                    
    return code_list, tuple(constants)


""" End of copypasta. """

if __name__ == "__main__":
    code_object = factorial.__code__

    pprint(find_linestarts(code_object))
    dis.dis(factorial)
