from subprocess import check_call, PIPE, Popen
import re

julia = r"""using CamJulia
using MLStyle

rmlines = @λ begin
    Expr(head, args...) -> Expr(head, filter(x -> x !== nothing, map(rmlines, args))...)
    ::LineNumberNode -> nothing
    a -> a
end

macro load_cam(path)
    aeson = CamJulia.ReadIR.load_aeson(path);
    ir = CamJulia.ReadIR.aeson_to_ir(aeson)
    x = CamJulia.CAM.ir_to_julia(ir)
    # @info rmlines(x)
    esc(x)
end

# @load_cam "./test.cam"
@load_cam "{}"
"""

python = r"""import sys
import os
import json
import unittest
sys.path.append('..')

from idris_cam.read_ir import *

def load_cam(path, session):
    with open(path, 'r') as f:
        js = json.load(f)

    letrec: LetRec = aeson_to_ir(js)
    return run_code(letrec, session)

class Test(unittest.TestCase):
    def test_too_known(self):
        sess = LinkSession()
        # load_cam('./test.cam', sess)
        load_cam({}, sess)
unittest.main()
"""

def load_cam_and_test(path):
    with open('../cam-julia/test/runtests.jl', 'w') as f:
        f.write(julia.format(path.strip()))
    with open('../cam-python/test/test.py', 'w') as f:
        f.write(python.format(repr(path.strip())))

get_exe = re.compile(r"Uncaught error\:(?P<exe>[\s\S]+)\: rawSystem").search

a = Popen(['stack', 'exec', 'idris', '--', '--checkpkg=cam.ipkg'], stdout=PIPE, stderr=PIPE)
print(a.stderr.read().decode())

a = Popen(['stack', 'exec', 'idris', '--', '--testpkg=cam.ipkg', '--codegen=cam'], stderr=PIPE, stdout=PIPE)

a.wait()
err_msg = a.stdout.read().decode()
x = get_exe(err_msg).group('exe')
load_cam_and_test(x)
