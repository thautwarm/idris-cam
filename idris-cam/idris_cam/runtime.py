"""
Some staging objects for compilations of common abstract machine.
"""
import operator
import numpy as np
from functools import update_wrapper


def raise_(*args):
    raise RuntimeError(args)

def throw(e):
    raise Exception(e)

def str_concat(s1, s2):
    return s1 + s2



rt_support = {
    'idris-python-rt.cmp': operator.eq,
    'idris-python-rt.check_case': lambda a, b, c, d: a == b and c == d,

    'prim-plus': lambda a,b : a + b,
    'prim-minus': operator.sub,
    'prim-times': operator.mul,
    'prim-udiv': operator.floordiv,
    'prim-sdiv': operator.floordiv,
    'prim-urem': operator.mod,
    'prim-srem': operator.mod,
    'prim-eq': operator.eq,
    'prim-slt': operator.lt,
    'prim-sle': operator.le,
    'prim-sgt': operator.gt,
    'prim-sge': operator.ge,


    'prim-and': operator.and_,  # not sure if it's bitwise operation or logic operation.
    'prim-or': operator.or_,

    # STR METHOD1
    'prim-streq': operator.eq,
    'prim-strlen': len,
    'prim-strindex': operator.getitem,
    'prim-strlt': operator.lt,

    # EFFECT
    "prim-external": raise_,
    'prim-writestr': lambda a, b: print(a, b, end=''),  # for supporting some rich consoles.
    'prim-readstr': input,

    # CONVERSION
    'prim-floatstr': str,
    'prim-strfloat': float,
    'prim-intstr': str,
    'prim-strint': int,
    'prim-intch': chr,
    'prim-chint': ord,

    'prim-sext': lambda x: x,
    'prim-zext': lambda x: x,

    # STR METHOD2
    'prim-strhead': lambda s: s[0],
    'prim-strtail': lambda s: s[1:],
    'prim-strcons': lambda a, b: a + b if b else a,
    'prim-strconcat': str_concat,
    'prim-crash': throw,
}
