module Main where
  import System.Environment

  main :: IO ()
  main = do
    putStrLn ("What is your name?")
    name <- getLine
    putStrLn ("Hello, " ++ name ++ "! Pleased to meet you. :)")

--    putStrLn (show (read (args !! 0 + (read (args !! 1)))))