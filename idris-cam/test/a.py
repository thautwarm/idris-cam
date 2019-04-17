from idris_cam.read_ir import *
import json

with open('../../examples/a.cam', 'r') as f:
    js = json.load(f)

letrec: LetRec = aeson_to_ir(js)
# letrec.dump(0, None)
# print()
run_code(letrec)
