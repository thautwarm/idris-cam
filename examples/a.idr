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

