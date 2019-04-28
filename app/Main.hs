module Main where

import Lib
import Control.Monad

main :: IO ()
main =
  do
    putStrLn "Welcome!"
    loop $ World 0 0 True 0 []
    putStrLn "Goodbye!"

data World =
  World {
      x :: Int
    , y :: Int
    , alive :: Bool
    , time :: Int
    , timeouts :: [Timeout]
  }

data Timeout =
  Timeout {
      name :: String
    , run :: World -> World
    , due :: World -> Bool
  }

instance Show World where
  show w =
            "X = "    ++ (show $ x w) ++
    ", " ++ "Y = "    ++ (show $ y w) ++ 
    ", " ++ "time = " ++ (show $ time w)

data Event =
  U | D | L | R | Q | H
  deriving (Eq, Show, Read)

loop :: World -> IO ()
loop w1 = do
  putStrLn $ show w1
  let dues = dueTimeouts w1
  forM_ (fmap name dues) putStrLn
  let w2 = runTimeouts w1 dues
  e <- events
  let w3 = think w2 e 
  if alive w3 then
    loop w3 { time = (time w3) + 1 }
  else
    return ()

dueTimeouts :: World -> [Timeout]
dueTimeouts w =
  let ts = timeouts w
  in filter (flip due $ w) ts

runTimeouts :: World -> [Timeout] -> World
runTimeouts w dues =
  let runs = fmap run dues
  in foldl (flip ($)) w runs 

events :: IO [Event]
events = do
  putStr "> "
  l <- getLine
  return $ fmap (\c -> read [c]) l

think :: World -> [Event] -> World
think w es = foldl apply w es 

apply :: World -> Event -> World
apply w U = w { y = (y w) + 1 }
apply w D = w { y = (y w) - 1 }
apply w L = w { x = (x w) - 1 }
apply w R = w { x = (x w) + 1 }
apply w Q = w { alive = False }
apply w H = w { timeouts = [jump w] ++ (timeouts w) }

jump :: World -> Timeout
jump w0 = Timeout {
      name = "Jumping..."
    , run = \w -> w { y = (y w) + 3 }
    , due = \w -> (time w) - (time w0) == 1
  }
