import sys
import os
print(os.getcwd())
sys.path.append('cam-python')
from idris_cam.read_ir import *
import json

with open('examples/test_ffi.cam', 'r') as f:
    js = json.load(f)

letrec: LetRec = aeson_to_ir(js)
# a = dict(letrec.seqs)
# print(a['Main.println'])
# print(a['Main.main'])


#.dump(0, None)
# print()
run_code(letrec)
