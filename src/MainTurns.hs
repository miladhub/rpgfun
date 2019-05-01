module MainTurns where

import RpgFun
import Control.Monad

game :: World -> IO ()
game = loop

loop :: World -> IO ()
loop w = do
  putStrLn $ show w
  timeoutEvents <- runTimeouts w
  userEvent <- userInput
  let w' = foldl think w $ userEvent : (timeoutEvents ++ [T])
  if alive w' then
    loop w'
  else
    return ()

