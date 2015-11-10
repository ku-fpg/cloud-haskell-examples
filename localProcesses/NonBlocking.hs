import Network.Transport.TCP (createTransport, defaultTCPParameters)
import Control.Distributed.Process
import Control.Distributed.Process.Node
import Control.Monad (forever)
import Control.Concurrent (threadDelay)

-- Receive Order - ProcessID
--                 Int
--                 String
--                 _____ 
listen = do
      third <- expect :: Process ProcessId
      second <- expectTimeout 1000 :: Process (Maybe Int)
      first <- expect :: Process String
      mapM_ (say) [(show first), (show second), (show third)]
      send third ()

--Send Order - Int 
--             String
--             ProcessId
--             

demo :: Process ()
demo = do
    listener <- spawnLocal $ listen
    send listener (3 :: Int)
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


