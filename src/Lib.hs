module Lib where

import Control.Monad (unless)
import Pipes
import System.IO (isEOF)

--         +--------+-- A 'Producer' that yields 'String's
--         |        |
--         |        |      +-- Every monad transformer has a base monad.
--         |        |      |   This time the base monad is 'IO'.
--         |        |      |  
--         |        |      |  +-- Every monadic action has a return value.
--         |        |      |  |   This action returns '()' when finished
--         v        v      v  v
stdinLn :: Producer String IO ()
stdinLn = do
    eof <- lift isEOF        -- 'lift' an 'IO' action from the base monad
    unless eof $ do
        str <- lift getLine
        yield str            -- 'yield' the 'String'
        stdinLn              -- Loop

foo :: Producer String IO ()
foo = yield "foo"

bar :: Consumer String IO ()
bar = do
  f <- await
  lift $ putStrLn f

eff :: String -> Effect IO ()
eff s = lift $ putStrLn s

someFunc :: IO ()
someFunc = putStrLn "someFunc"
