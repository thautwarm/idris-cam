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


def str_cons(s1, s2):
    return s1 + s2


def write_str(io, a):
    print(a, end='', file=io)


def cmp(a, b):
    return a == b

rt_support = {
    'cam-rt.cmp': cmp,
    'cam-rt.is': operator.is_,
    'cam-rt.err': raise_,

    'prim-plus': operator.add,
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
    'prim-and':
    operator.and_,  # not sure if it's bitwise operation or logic operation.
    'prim-or': operator.or_,

    # STR METHOD1
    'prim-streq': operator.eq,
    'prim-strlen': len,
    'prim-strindex': operator.getitem,
    'prim-strlt': operator.lt,

    # EFFECT
    "prim-external": raise_,
    'prim-writestr': write_str,  # for supporting some rich consoles.
    'prim-readstr': input,

    # CONVERSION
    'prim-floatstr': str,
    'prim-strfloat': float,
    'prim-intstr': str,
    'prim-strint': int,
    'prim-intch': chr,
    'prim-chint': ord,
    # 'prim-sext': lambda x: x,
    # 'prim-zext': lambda x: x,

    # STR METHOD2
    'prim-strhead': lambda s: s[0],
    'prim-strtail': lambda s: s[1:],
    'prim-strcons': str_cons,
    'prim-strconcat': str_concat,
    'prim-crash': throw,

    'builtin-getattr': getattr,
    'builtin-map': map,
    'builtin-println': print,
    'builtin-simple_open': open,
    'builtin-simple_read': lambda x: x.read()
}
