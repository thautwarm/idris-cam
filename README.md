# idris-cam

A framework for Idris RTS.


## Features(listed by priorities)

- [x] An abstraction of some intermediate representations(common abstract machine, aka CAM)
- [x] Back end: Python AST
- [ ] Back end: Julia AST
- [ ] Persisting locations with Idris IRs, like DDecls.
- [ ] Python standard libraries
- [x] Handy FFI
- [ ] Tail call elimation
- [ ] Back end: Python Bytecode
- [ ] Specializations for some primitive data types
- [ ] Incremental compilation

## Build and Run

- Build

    ```
    git clone https://github.com/thautwarm/idris-cam && cd idris-cam
    stack build
    ```

### Run

Write an Idris script, named `a.idr`:

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

Finally, write a Python script, append `$Project/cam-python` into **PYTHONPATH**, like:

```python
import sys
sys.path.append('cam-python')

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

## FFI Mechansim


The task about [New Foreign Function Interface](http://docs.idris-lang.org/en/latest/reference/ffi.html)
makes this handy FFI feasible.

## Builtin


### Idris Side

In Idris side, we have made a FFI implementation for Idris-CAM, and then you can
declare foreign functions via such codes:

```idris
println : String -> CamIO ()
println s = camCall (String -> CamIO ()) (Builtin "println") s

data FileHandler;

openFile : String -> CamIO (Com_Raw FileHandler)
openFile filename = camCall (String -> CamIO (Com_Raw FileHandler)) (Builtin "simple_open") filename

readFile : Com_Raw FileHandler -> CamIO String
readFile handle = camCall (Com_Raw FileHandler -> CamIO String) (Builtin "simple_read") handle
```

Above codes are placed at [examples/test_ffi.cam](https://github.com/thautwarm/idris-cam/blob/master/examples/test_ffi.idr), and I'll make Idris libraries for both Julia and Python sooner.


### Target Back End

We have already Julia and Python back ends, and Python's is already available, so we use Python to demonstrate how
to setup FFI.

Note that we referenced `println`, `simple_open` and `simple_read` as builtin functions in previous Idris codes.

So we update `cam-python/idris-cam/runtime.py`, add their
implementations:

```python
rt_support = {
    'idris-cam-rt.cmp': cmp,

    'prim-plus': operator.add,
    'prim-minus': operator.sub,
    ...
    'builtin-println': print,
    'builtin-simple_open': open,
    'builtin-simple_read': lambda x: x.read()
}
```

### Test your codes

```
stack exec idris -- --codegen=cam ./example/test_ffi -o ./example/test_ffi.cam

python cam-python/test/a.py
```