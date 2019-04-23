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
import Data.Vect
import Data.HVect

-- hide them for we have adhoc `index` from type class Indexable!
%hide HVect.index
%hide Vect.index

%access export

testSimple : FFI.IO ()
testSimple = do
      println $ show hvec2
      println $ show a
      println $ show reva
      println $ show e
      println $ show hvec
      println $ show hvecItem
  where
        a : Vect 3 Int
        a = [1, 2, 3]
        nnn : Nat
        nnn = f (MkTypeHolder (Vect 3 Int))

        reva : Vect 3 Int
        reva = reverse a

        e : Int
        e = index (the (Fin _) $ fromInteger 1) a

        hvec : HVect [Int, Double]
        hvec = [1, 2.0]

        hvecItem : Double
        hvecItem = index (the (Fin _) $ fromInteger 1) hvec

        hvec2 : HVect [Double, Int]
        hvec2 = reverse hvec
```

### Test in Python

```

~/github/idris-cam | master> cd cam-python/test && python test.py

[2,1]
[1, 2, 3]
[3, 2, 1]
2
[1,2]
2

.
----------------------------------------------------------------------
Ran 1 test in 0.175s

OK
```

### Test in Julia

```
~/github/idris-cam | master> cd cam-julia
~/github/idris-cam | master> julia
~/github/idris-cam | master> cd cam-julia
julia> ]
v1.1> activate .
CamJulia> test

[2.0,1]
[1, 2, 3]
[3, 2, 1]
2
[1,2.0]
2.0
```



## FFI Mechansim


The task about [New Foreign Function Interface](http://docs.idris-lang.org/en/latest/reference/ffi.html)
makes this handy FFI feasible.

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