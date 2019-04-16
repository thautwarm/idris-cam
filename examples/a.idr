module Main

a : Int -> Int
a x = x + 1

public export
main : IO ()
main = do
    putStrLn $ "ahhhh"  ++ show (a 1)
    pure ()