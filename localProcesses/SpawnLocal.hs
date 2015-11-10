import Network.Transport.TCP (createTransport, defaultTCPParameters)
import Control.Distributed.Process
import Control.Distributed.Process.Node
import Control.Monad (forever)
import Control.Concurrent (threadDelay)

replyBack :: (ProcessId, String) -> Process ()
replyBack (sender, msg) = send sender msg

logMessage :: String -> Process ()
logMessage msg = say $ "handling " ++ msg



main :: IO ()
main = do
   Right t <- createTransport "127.0.0.1" "10501" defaultTCPParameters
   node <- newLocalNode t initRemoteTable
   _ <- forkProcess node $ do
          -- spawn worker on local node
          echoPid <- spawnLocal $ forever $ do
             -- worker will match on logMessage or replyBack
             receiveWait [match logMessage, match replyBack]
          say "send some messages!"
          
          -- message is just a string,  logMessage will match 
          send echoPid "hello"
          self <- getSelfPid
          
          -- message is a (respId, Msg), replyBack will match 
          send echoPid (self, "hello")
          send echoPid "HEYHEYHEY"
          -- expect a message or timeout doesn't seem to work...
          m <- expectTimeout 1000000

          case m of
            Nothing -> die "nothing came back!"
            (Just s ) -> say $ "got " ++ s ++ " back!"
          return ()
   liftIO $ threadDelay (1*1000000) 
   return ()


