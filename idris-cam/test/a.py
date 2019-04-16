from idris_cam.read_ir import *
import json

with open('../../examples/a.pyast', 'r') as f:
    js = json.load(f)

letrec: LetRec = aeson_to_ir(js)
#
for k, v in letrec.seqs:
    print(k, '=>', v)
print(run_code(letrec))
