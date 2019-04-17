# idris-cam

A framework for Idris RTS.

## Build and Run

- Build

    ```
    git clone https://github.com/thautwarm/idris-cam && cd idris-cam
    stack build
    ```

### Run

Write a Idris script, named `a.idr`:

```idris
module Main
import Data.Vect

public export

len : Vect n a -> Int
len {n=Z} [] = 0
len {n=S k} (with Vect x::xs) = 1 + len xs

v : Vect ?n Int
v = [1, 2, 3]

main : IO ()
main = do
    putStrLn . show $ len v
    putStrLn .show  $ with List ["a"]
    pure ()
```

Then compile it to CAM IR.
```
stack exec idris -- --codegen=cam ./examples/a.idr -o ./examples/a.cam
```

Finally, write a Python script, append `$Project/idris-cam` into **PYTHONPATH**, like:

```python
import sys
sys.path.append('idris-cam')

from idris_cam.read_ir import *
import json

with open('examples/a.cam', 'r') as f:
    js = json.load(f)

letrec: LetRec = aeson_to_ir(js)
# uncomment to show pretty printed CAM IRs.
# letrec.dump(0, None)
# print()
run_code(letrec)
```
which produces
```
3
["a"]
```



## Features(listed by priorities)

- [x] An abstraction of some immediate representations(common abstract machine, aka CAM)
- [x] Back end: Python AST
- [ ] Back end: Julia AST
- [ ] Persisting locations with Idris IRs, like DDecls.
- [ ] Python standard libraries
- [ ] FFI
- [ ] Tail call emilation
- [ ] Back end: Python Bytecode
- [ ] Specializations for some primitive data types
- [ ] Incremental compilation
