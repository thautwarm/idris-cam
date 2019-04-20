from subprocess import check_call, PIPE, Popen
import re


def load_cam_and_test(path):
    print(f'Find cam file {path}, prepare to test.')

get_exe = re.compile(r"Uncaught error(?P<exe>[\s\S]+)rawSystem").search

a = Popen(['stack', 'exec', 'idris', '--', '--checkpkg=cam.ipkg'], stdout=PIPE, stderr=PIPE)
print(a.stderr.read().decode())

a = Popen(['stack', 'exec', 'idris', '--', '--testpkg=cam.ipkg', '--codegen=cam'], stderr=PIPE, stdout=PIPE)

a.wait()
err_msg = a.stdout.read().decode()
x = get_exe(err_msg).group('exe')
load_cam_and_test(x)