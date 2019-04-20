module Main

fred : (String, Int)
fred = ("Fred", 42)

main : IO ()
main = do
    putStrLn . show $ fred
    pure ()