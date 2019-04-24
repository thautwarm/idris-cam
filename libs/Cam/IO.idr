module IO
import Cam.FFI

||| print native(idris side) object
public export
%inline
println : a -> IO ()
println s = believe_me $ fcall FFun1 (Builtin "println") $ believe_me s

||| print any foreign object
public export
%inline
fprintln : Boxed a -> IO ()
fprintln boxed = believe_me $ fcall FFun1 (Builtin "println") $ believe_me boxed

||| print any object
public export
%inline
putStrLn : String -> IO ()
putStrLn boxed = believe_me $ fcall FFun1 (Builtin "println") $ believe_me boxed


public export
%inline
toText : a -> Boxed String
toText a = believe_me . unsafePerformIO $ fcall FFun1 (Builtin "to_text") $ believe_me a
