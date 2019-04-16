import json
from dataclasses import dataclass
from typing import *
from idris_cam.abstract_machine import *
from idris_cam.runtime import rt_support


def list_conv(f, xs):
    return list(map(aeson_to_ir, xs))

def com_let(cs):
    return Let(cs[0], aeson_to_ir(cs[1]), aeson_to_ir(cs[2]))

def com_letrec(cs):
    return LetRec(
        [(fst, aeson_to_ir(snd)) for [fst, snd] in cs[0]],
        aeson_to_ir(cs[1])
    )

def com_if(cs):
    return If(*list_conv(cs))

def com_while(cs):
    return While(*list_conv(cs))

def com_mutate(cs):
    return Mutate(cs[0], aeson_to_ir(cs[1]))

def com_fun(cs):
    return Fun(cs[0], aeson_to_ir(cs[1]))

def com_app(cs):
    return App(aeson_to_ir(cs[0]), list_conv(cs[1]))

def com_var(cs):
    return Var(cs[0])

def com_block(cs):
    return BlockExpr(list_conv(cs[0]))

def com_tuple(cs):
    return DataStructure.Tuple(BlockExpr(list_conv(cs[0])))

def com_proj(cs):
    return Proj(*list_conv(cs))

def com_const(cs):
    return Const(cs[0])

def com_internal(cs):
    return Staged(rt_support[cs[0]])


switch = {
    'ComLet': com_let,
    'ComLetrec': com_letrec,
    'ComIf': com_if,
    'ComWhile': com_while,
    'ComMutate': com_mutate,
    'ComFun': com_fun,
    'ComApp': com_app,
    'ComVar': com_var,
    'ComBlock': com_block,
    'ComTuple': com_tuple,
    'ComProj': com_proj,

    'ComInt': com_const,
    'ComDouble': com_const,
    'ComCh': com_const,
    'ComBool': com_const,
    'ComNil': None,

    'ComInternal': com_internal
}

def aeson_to_ir(d, switch=switch):
    action = switch(d['tag'])
    if not action: # ComNil
        return None
    return action(d['contents'])