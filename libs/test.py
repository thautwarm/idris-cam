from subprocess import check_call, PIPE, Popen
import re

s = r"""
using CamJulia
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
def load_cam_and_test(path):
    with open('../cam-julia/test/runtests.jl', 'w') as f:
        f.write(s.format(path.strip()))

get_exe = re.compile(r"Uncaught error\:(?P<exe>[\s\S]+)\: rawSystem").search

a = Popen(['stack', 'exec', 'idris', '--', '--checkpkg=cam.ipkg'], stdout=PIPE, stderr=PIPE)
print(a.stderr.read().decode())

a = Popen(['stack', 'exec', 'idris', '--', '--testpkg=cam.ipkg', '--codegen=cam'], stderr=PIPE, stdout=PIPE)

a.wait()
err_msg = a.stdout.read().decode()
x = get_exe(err_msg).group('exe')
load_cam_and_test(x)