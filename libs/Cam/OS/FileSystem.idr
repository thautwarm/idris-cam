module Cam.FileSystem
import Cam.FFI
import Cam.Data.FCollections
import Cam.Data.Compat
import Data.Vect
import Data.HVect

%access export
public export
%inline

pipe : () -> FHVect [Int, Int]
pipe () = believe_me . unsafePerformIO $ fcall1 "filesystem_pipe" $ believe_me ()

public export
%inline
getcwd : () ->  Boxed String
getcwd () = believe_me . unsafePerformIO $ fcall1 "filesystem_getcwd" $ believe_me ()

data FileHandlerSpec;

public export
FileHandler : Type
FileHandler = Boxed FileHandlerSpec

public export
%inline
openFile : (filename: String) -> (mode: String) ->  IO FileHandler
openFile filename mode =
          let filename = believe_me . the (Boxed String) $ toForeign filename in
          let mode = believe_me . the (Boxed String) $ toForeign mode in
          believe_me $ fcall2 "filesystem_open_file" filename mode

public export
%inline
closeFile : FileHandler ->  IO ()
closeFile f =
        let f = believe_me f in
        believe_me $ fcall1 "filesystem_close_file" f

public export
%inline
readAllText : FileHandler -> IO (Boxed String)
readAllText f =
          let f = believe_me f in
          believe_me $ fcall1 "filesystem_read_all_text" f
