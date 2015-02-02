import Network.Transport.TCP (createTransport, defaultTCPParameters)
import Control.Distributed.Process
import Control.Distributed.Process.Node
import Control.Monad (forever)
import Control.Concurrent (threadDelay)

-- receive Order - ProcessID
--                 String
--                 _____ 
listen = do
      third <- expect :: Process ProcessId
      first <- expect :: Process String
      second <- expectTimeout 100000 :: Process (Maybe String)
      mapM_ (say) [(show first), (show second), (show third)]
      send third ()

--Send Order - String
--             ProcessId
--             

demo :: Process ()
demo = do
    listener <- spawnLocal listen
    send listener "hello"
    getSelfPid >>= send listener
    () <- expect
    return ()


-- Demo sends things out of order, but we don't block
main :: IO ()
main = do
   Right t <- createTransport "127.0.0.1" "10501" defaultTCPParameters
   node <- newLocalNode t initRemoteTable
   _ <- forkProcess node $ do
          demo
          return ()
   liftIO $ threadDelay (1*1000000) 
   return ()


