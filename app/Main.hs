module Main where

import Lib

main :: IO ()
main =
  do
    putStrLn "Welcome!"
    loop $ World 0 0 True
    putStrLn "Goodbye!"

data World =
  World {
      x :: Int
    , y :: Int
    , alive :: Bool
  }
  deriving (Eq, Show)

data Event =
  U | D | L | R | Q
  deriving (Eq, Show, Read)

loop :: World -> IO ()
loop w0 = do
  putStrLn $ show w0
  e <- events
  let w1 = think w0 e 
  if alive w1 then
    loop w1
  else
    return ()

events :: IO [Event]
events = do
  putStr "> "
  l <- getLine
  return $ fmap (\c -> read [c]) l

think :: World -> [Event] -> World
think w es = foldl apply w es 

apply :: World -> Event -> World
apply (World x y a) U = World x (y + 1) a
apply (World x y a) D = World x (y - 1) a
apply (World x y a) L = World (x - 1) y a
apply (World x y a) R = World (x + 1) y a
apply (World x y _) Q = World x y False
