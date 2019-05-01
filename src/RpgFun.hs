module RpgFun where

import Control.Monad

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
    , run :: World -> Event
    , due :: World -> Bool
  }

instance Show World where
  show w =
            "X = "    ++ (show $ x w) ++
    ", " ++ "Y = "    ++ (show $ y w) ++
    ", " ++ "time = " ++ (show $ time w)

data Event =
  U | D | L | R | Q | H | T
  deriving (Eq, Show, Read)

userInput :: IO Event
userInput = do
  l <- getLine
  return $ read l

think :: World -> Event -> World
think w U = w { y = (y w) + 1 }
think w D = w { y = (y w) - 1 }
think w L = w { x = (x w) - 1 }
think w R = w { x = (x w) + 1 }
think w Q = w { alive = False }
think w H = w { timeouts = [jump w] ++ (timeouts w) }
think w T = w { time = (time w) + 1 }

runTimeouts :: World -> IO [Event]
runTimeouts w = do
  let ts = timeouts w
      dues = filter (flip due $ w) ts
      runs = fmap run dues
      events = runs <*> pure w
  forM_ (fmap name dues) putStrLn
  return events

jump :: World -> Timeout
jump w0 = Timeout {
      name = "Jumping..."
    , run = \_ -> U
    , due = \w -> (time w) - (time w0) == 1
  }
