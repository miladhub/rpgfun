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

handler :: World -> Consumer Event IO ()
handler = loop
  where
    loop w = do
      lift $ putStrLn $ show w
      worldEvents <- lift $ runTimeouts w
      userEvent <- await
      let w' = foldl think w (userEvent : worldEvents)
      if alive w' then
        loop w'
      else
        return ()

game :: World -> IO ()
game w = do
    (output, input) <- spawn unbounded

    forkIO $ do runEffect $ user >~ toOutput output
                performGC

    forkIO $ do runEffect $ tick >-> toOutput output
                performGC
    
    runEffect $ fromInput input >-> (handler w)

