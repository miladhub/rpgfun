module MainTurns where

import RpgFun
import Control.Monad

game :: World -> IO ()
game = loop

loop :: World -> IO ()
loop w = do
  putStrLn $ show w
  worldEvents <- runTimeouts w
  userEvent <- userInput
  let w' = foldl think w (userEvent : (worldEvents ++ [T]))
  if alive w' then
    loop w'
  else
    return ()

