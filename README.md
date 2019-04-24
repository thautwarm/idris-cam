# idris-cam

A framework for Idris RTS.


## Features(listed by priorities)

- [x] An abstraction of some intermediate representations(common abstract machine, aka CAM)
- [x] Back end: Python AST
- [x] Back end: Julia AST
- [ ] Persisting locations with Idris IRs, like DDecls.
- [ ] Python standard libraries
- [x] Handy FFI
- [ ] Tail call elimination
- [ ] Back end: Python Bytecode
- [ ] Specializations for some primitive data types
- [ ] Incremental compilation

## Build && Cam Codegen

- Build

    ```
    git clone https://github.com/thautwarm/idris-cam && cd idris-cam
    stack build
    ```

- Codegen

    ```
    stack exec idris -- --codegen=cam <input idris files> -o <output cam file>
    ```


## Python & Julia Example


### Test Generation

You can test the Idris package `Cam`, generating test files for Python or Julia with

```
~/github/idris-cam | master> cd libs && python gen_test.py
```

Above command compiles `idris-cam/libs/Test/Simple.idr` into `.cam` file.

```idris
-- idris-cam/libs/Test/Simple.idr
module Test.Simple
import Cam.FFI
import Cam.OS.FileSystem
import Cam.IO
import Cam.Data.Collections
import Cam.Data.FCollections
import Cam.Data.Compat
import Data.Vect
import Data.HVect

%hide HVect.index
%hide Vect.index
%hide Vect.reverse

%access export

f : StaticSized c => TypeHolder c -> Nat
f d = typeSize d

testSimple : FFI.IO ()
testSimple = do
      -- In Julia, use "MLStyle" or other Julia module.
      sklearn <- camImport $ TheModule "sklearn"

      -- fprintln works for only foreign objects
      fprintln sklearn

      -- get property of module
      external_mod <- camImportFrom sklearn "externals"

      -- println works for all objects
      println external_mod

      file <- openFile "./text.txt" "r"
      text <- readAllText file
      closeFile file
      fprintln $ text
      println $ "test hvec: " show hvec
    where
      hvec : HVect [Double, Int]
      hvec = reverse [1.0, 5]
```

### Test in Python

```

~/github/idris-cam | master> cd cam-python/test && python test.py

<module 'sklearn' from '/home/redbq/Software/Anaconda/lib/python3.7/site-packages/sklearn/__init__.py'>
<module 'sklearn.externals' from '/home/redbq/Software/Anaconda/lib/python3.7/site-packages/sklearn/externals/__init__.py'>
啊，太懂了！太懂Idris Python辣！

test hvec: [1.0, 5]
.
----------------------------------------------------------------------
Ran 1 test in 0.177s

OK
```

### Test in Julia

```
~/github/idris-cam | master> cd cam-julia
~/github/idris-cam | master> julia
~/github/idris-cam | master> cd cam-julia
julia> ]
v1.1> activate .
(CamJulia) pkg> test
   Testing CamJulia
 Resolving package versions...

MLStyle
MLStyle.MatchCore.gen_match
啊，太懂了！太懂Idris Julia辣！

test hvec: [1.0, 5]
```


## Cam Loader

A `.cam` file is simply a JSON file consist of the abstract instructions which could be loaded by Python and Julia and compiled conveniently.

For Julia, there's a [`@load_cam` macro](https://github.com/thautwarm/idris-cam/blob/master/cam-julia/test/runtests.jl):

```julia
macro load_cam(path)
    aeson = CamJulia.ReadIR.load_aeson(path);
    ir = CamJulia.ReadIR.aeson_to_ir(aeson)
    x = CamJulia.CAM.ir_to_julia(ir)
    esc(x)
end

@load_cam "./test.cam"
```

For Python, there is a [`load_cal` function](https://github.com/thautwarm/idris-cam/blob/master/cam-python/test/test.py):

```julia
def load_cam(path, session):
    with open(path, 'r') as f:
        js = json.load(f)

    letrec: LetRec = aeson_to_ir(js)
    return run_code(letrec, session)
```

The `session` argument of Python's `load_cam` indicates a symbol generator. Comparisons of symbols are O(1) thus allow us to
create efficient tagged unions(ADTs, data types). In Julia, `Symbol` is an essential type.

## FFI Mechansim


Thanks to the tasks of [New Foreign Function Interface](http://docs.idris-lang.org/en/latest/reference/ffi.html)
, it makes our handy FFI feasible.

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