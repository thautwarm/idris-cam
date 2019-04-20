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


1. Check file [examples/test_ffi.idr](https://github.com/thautwarm/idris-cam/blob/master/examples/test_ffi.idr), the main is

    ```idris
    main : CamIO ()
    main = do
        fh <- openFile "./text.txt"
        s <- readFile fh
        println s
    ```

2. Generate `.cam` files via idris-codegen.

    ```
    stack exec idris -- --codegen=cam ./examples/test_ffi.idr -o ./cam-julia/test/test.cam

    stack exec idris -- --codegen=cam ./examples/test_ffi.idr -o ./cam-python/test/test.cam
    ```

3. Test Julia, check [can-julia/test/runtests.jl](https://github.com/thautwarm/idris-cam/tree/master/cam-julia/test/runtests.jl)

    ```julia
    using CamJulia

    module A
    end
    macro load_cam(path)
        aeson = CamJulia.ReadIR.load_aeson(path);

        ir = CamJulia.ReadIR.aeson_to_ir(aeson)

        CamJulia.CAM.ir_to_julia(ir)
    end

    @load_cam "./test.cam"
    ```

    You'd open julia-shell in `cam-julia` directory, then

    ```
    julia> ]
    pkg> activate .
    CamJulia> instantiate
    CamJulia> test
       Testing CamJulia
    Resolving package versions...
        啊，太懂了！太懂Idris Julia辣！
    Testing CamJulia tests passed
    ```

    The output text is from `text.txt` located in cam-julia/test/text.txt.

4. Test Python, check [cam-python/test/test.py](https://github.com/thautwarm/idris-cam/tree/master/cam-python/test/test.py),

    ```python
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
    ```

    Then go to directory `cam-python/test`, run
    ```
    python test.py

    啊，太懂了！太懂Idris Python辣！
    .
    ----------------------------------------------------------------------
    Ran 1 test in 0.086s

    OK
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