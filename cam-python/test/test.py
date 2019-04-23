import sys
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
        load_cam('/tmp/idris13046-1', sess)
unittest.main()
