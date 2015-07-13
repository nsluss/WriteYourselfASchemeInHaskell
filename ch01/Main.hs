module Main where
import System.Environment


--getLine is an IO action that reads a line from the console and returns it as
--a string. Change the program so it prompts for a name, reads the name, and then
--  prints that instead of the command line value

main :: IO ()
main = do
  putStrLn "What's your name?"
  name <- getLine
  putStrLn ("Hi, " ++ name ++ "!")