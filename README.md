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

- Install to `.local`

    ```
	stack install
	```

	After installing this, your `idris` command could use `cam` back end via `idris --codegen cam`.

- Usage within `idris` command.

    ```
    idris f1.idr f2.idr -o out.cam --codegen cam -p cam
    ```

- Run `.cam` files

    - Idris-Python

        You can check [Idris-Python](https://github.com/thautwarm/idris-python), and install it
        via [pip](https://pypi.org/project/pip/) will give you a command `run-cam` which accepts
        a filename of `.cam` file and executes it in Python.

        Also, you can load `.cam` file in Python session, via [load_cam function](https://github.com/thautwarm/idris-python/blob/master/idris_python/cli.py#L69).

    - Idris-Julia

        Check [cam-julia/test/runtests.jl](https://github.com/thautwarm/idris-cam/blob/master/cam-julia/test/runtests.jl).

        It hasn't been published yet.

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

Use `idris-python` command from [Idris-Python](https://github.com/thautwarm/idris-python). Change module name from `Test.Simple` to `Main` is okay.

```
idris-python main.idr
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

For Python, there is a [`load_cal` function](https://github.com/thautwarm/idris-python/blob/master/idris_python/loader.py):

```python
def load_cam(path, session):
    with open(path, 'r') as f:
        js = json.load(f)

    letrec: LetRec = aeson_to_ir(js)
    return run_code(letrec, session)
```

The `session` argument of Python's `load_cam` indicates a symbol generator. Comparisons of symbols are O(1) thus allow us to
create efficient tagged unions(ADTs, data types). In Julia, `Symbol` is an essential type.

## FFI Mechansim


Our handy FFI becomes feasible due to [New Foreign Function Interface](http://docs.idris-lang.org/en/latest/reference/ffi.html).

### Idris Side

In Idris side, we have made a FFI implementation for Idris-CAM, and then you can
declare foreign functions via such codes:

```idris
println : Unsafe -> FFI.IO Unsafe
println s = fcall (Unsafe -> FFI.IO Unsafe) (Builtin "println") s
```

You can check [libs/Cam](https://github.com/thautwarm/idris-cam/blob/master/libs/Cam) for more information, and I'll make Idris libraries for both Julia and Python sooner.


### Target Back End

We have already Julia and Python back ends, and Python's is already available, so we use Python to demonstrate how
to setup FFI.

Note that we referenced `println` as builtin functions in previous Idris codes.


So we update [runtime.py](https://github.com/thautwarm/idris-python/blob/0c4fdb523658c521e789944a12eb235b7f67dc23/idris_python/runtime.py#L99), add its
implementation:

```python
    return {
        'cam-rt.cmp': operator.eq,
        'cam-rt.is': operator.is_,
        ...
        'builtin-println': print,
        ...
}
```
