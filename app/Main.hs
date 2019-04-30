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
  w2 <- runTimeouts w1
  e <- events
  let w3 = think w2 e 
  if alive w3 then
    loop w3 { time = (time w3) + 1 }
  else
    return ()

runTimeouts :: World -> IO World
runTimeouts w = do
  let ts = timeouts w
      dues = filter (flip due $ w) ts
      runs = fmap run dues
      newWorld = foldl (flip ($)) w runs
  forM_ (fmap name dues) putStrLn
  return newWorld

events :: IO Event
events = do
  putStr "> "
  l <- getLine
  return $ read l

think :: World -> Event -> World
think w U = w { y = (y w) + 1 }
think w D = w { y = (y w) - 1 }
think w L = w { x = (x w) - 1 }
think w R = w { x = (x w) + 1 }
think w Q = w { alive = False }
think w H = w { timeouts = [jump w] ++ (timeouts w) }

jump :: World -> Timeout
jump w0 = Timeout {
      name = "Jumping..."
    , run = \w -> w { y = (y w) + 3 }
    , due = \w -> (time w) - (time w0) == 1
  }
