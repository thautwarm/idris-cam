from subprocess import check_call, PIPE, Popen
import re


def load_cam_and_test(path):
    print(f'Find cam file {path}, prepare to test.')

get_exe = re.compile(r"Type checking (?P<exe>\S+?)\.idr").match

a = Popen(['stack', 'exec', 'idris', '--', '--checkpkg=cam.ipkg'], stdout=PIPE, stderr=PIPE)
print(a.stderr.read().decode())

a = Popen(['stack', 'exec', 'idris', '--', '--testpkg=cam.ipkg'], stdout=PIPE, stderr=PIPE)

err_msg = a.stdout.read().decode()
load_cam_and_test(get_exe(err_msg).group('exe'))