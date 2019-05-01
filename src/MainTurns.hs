module MainTurns where

import RpgFun
import Control.Monad

game :: IO ()
game = loop $ World 0 0 True 0 []

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

runTimeouts :: World -> IO [Event]
runTimeouts w = do
  let ts = timeouts w
      dues = filter (flip due $ w) ts
      runs = fmap run dues
      events = runs <*> pure w
  forM_ (fmap name dues) putStrLn
  return events
