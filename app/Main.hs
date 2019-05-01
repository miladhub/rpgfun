module Main where

import MainTurns as T
import MainAsync as A
import RpgFun
import System.Environment

main :: IO ()
main = do
  args <- getArgs
  if (length args) /= 1
    then
      putStrLn "Wrong syntax"
    else do
      putStrLn "Welcome!"
      let w = World 0 0 True 0 []
      if (head args) == "A"
        then A.game w
        else T.game w
      putStrLn "Goodbye!"
