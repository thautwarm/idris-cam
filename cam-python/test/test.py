import sys
import os
import json
import unittest
sys.path.append('..')

from idris_cam.read_ir import *
class Test(unittest.TestCase):
    def test_too_known(self):
        with open('./test.cam', 'r') as f:
            js = json.load(f)

        letrec: LetRec = aeson_to_ir(js)
        # a = dict(letrec.seqs)
        # print(a['Main.println'])
        # print(a['Main.main'])


        #.dump(0, None)
        # print()
        run_code(letrec)
        return

unittest.main()