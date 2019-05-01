module MainAsync where

import RpgFun
import Pipes
import Pipes.Concurrent
import Control.Concurrent (threadDelay)
import Control.Monad (forever)

user :: Effect IO Event
user = lift userInput

tick :: Producer Event IO r
tick = forever $ do
    lift $ threadDelay 1000000
    yield T

handler :: Consumer Event IO ()
handler = loop $ World 0 0 True 0 []
  where
    loop w = do
      lift $ putStrLn $ show w
      e <- await
      let w' = think w e
      if alive w' then
        loop w'
      else
        return ()

game :: IO ()
game = do
    (output, input) <- spawn unbounded

    forkIO $ do runEffect $ user >~ toOutput output
                performGC

    forkIO $ do runEffect $ tick >-> toOutput output
                performGC
    
    runEffect $ fromInput input >-> handler

