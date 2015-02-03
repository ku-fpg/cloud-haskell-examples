{-# LANGUAGE TemplateHaskell, DeriveDataTypeable, DeriveGeneric #-}
{-# OPTIONS_GHC -Wall #-}
import Control.Distributed.Process
import Control.Distributed.Process.Closure
import Control.Concurrent

import Control.Monad
import Text.Printf
import GHC.Generics (Generic)
import Data.Binary
import Data.Typeable

import DistribUtils
type Sender = ProcessId
type Peer = ProcessId
type Master = ProcessId

-- <<Message
data Terminate = Terminate
  deriving (Typeable, Generic)

instance Binary Terminate

data ForwardAndRespond = Forward(Sender, Peer, PeerToPeer)
  deriving (Typeable, Generic)

instance Binary ForwardAndRespond

data PeerToPeer = Chat(Sender,String,Master)
                | Fib (Sender, Int)
                | Fac (Sender, Int)
                | Answer (Sender, Int)
  deriving (Typeable, Generic)

instance Binary PeerToPeer              

data MessageJ = Ping Sender
             | Pong Sender
             | StartChat (Sender,Peer)
  deriving (Typeable, Generic)          -- <1>

instance Binary MessageJ                 -- <2>
-- >>
fib:: Int->Int
fib n  
   |n < 2 = 1
   | otherwise = (fib (n-2)) + (fib (n-1)) 


fac :: Int->Int
fac n
  | n < 1 = 1
  | otherwise = n * fac (n-1)

forwardHandler :: ForwardAndRespond -> Process()
forwardHandler m = do case m of
                        Forward (_, to, payload) -> do say $ "received forward request"
                                                       send to payload

peerToPeerHandler :: PeerToPeer -> Process ()
peerToPeerHandler m = do case m of 
                           Chat (from, msg, master) -> chatFunc from msg master 
                           Fib  (from, i) -> fibFunc from i
                           Fac  (from , i) -> facFunc from i
                           Answer ( _, i) -> ansFunc i
messageHandler :: MessageJ -> Process()
messageHandler m = do mypid<- getSelfPid
                      case m of 
                        Ping from -> do say $ printf "ping received from %s" (show from) -- <2>
                                        send from (Pong mypid)                           -- <4>
                        Pong from-> do say $ "Error Pong Message received"
                                       send from (Pong mypid) 
                        StartChat (master, peer)-> do say $ "Start chat with "++ show(peer)
                                                      send peer (Chat(mypid, "Hello",master))

chatFunc :: ProcessId -> String-> Master -> Process ()
chatFunc to msg master = do mypid <- getSelfPid
                            say $ "Received "++ msg
                            liftIO $ threadDelay (100000)
                            case msg of
                                (_:xs)-> send to (Chat (mypid,xs,master))
                                []-> send master Terminate

fibFunc :: ProcessId -> Int -> Process ()
fibFunc to i = do mypid<- getSelfPid
                  say $ printf "Fib received from %s" (show to)
                  send to (Answer (mypid,(fib i)))

facFunc :: ProcessId -> Int -> Process()

facFunc to i = do mypid<- getSelfPid
                  say $ printf "Fac received from %s" (show to)
                  send to (Answer (mypid,(fac i)))

ansFunc ::Int -> Process()
ansFunc i = say $ "Result: "++(show i)

fibServer :: Process()
fibServer = forever $ do receiveWait [match peerToPeerHandler, match messageHandler, match forwardHandler]


-- <<remotable
remotable ['fibServer]
-- >>

-- <<master
master :: [NodeId] -> Process ()                     -- <1>
master peers = do

  ps <- forM peers $ \nid -> do                      -- <2>
          say $ printf "spawning on %s" (show nid)
          spawn nid $(mkStaticClosure 'fibServer)

  mypid <- getSelfPid
  forM_ ps $ \pid -> do
    say $ printf "pinging %s" (show pid)
    send pid (Ping mypid)
  
  waitForPongs ps
  liftIO $ threadDelay 50000 
  liftIO $ putStrLn "Enter a number for fib: "
  x<- liftIO $ getLine 
  send (head ps) (Fib (mypid, read x ::Int))
  receiveWait [match peerToPeerHandler]

  liftIO $ threadDelay 50000 
  liftIO $ putStrLn "Enter a number for fac to be forwarded on: "
  y<- liftIO $ getLine 
  
    
  send ((head . tail) ps) (Forward (mypid, (head ps), (Fac (mypid, (read y :: Int) ) ))) 
  
  receiveWait [match peerToPeerHandler]
  liftIO $ threadDelay 50000 

  say $ "Sending StartChat mesg" 
  send (head ps) (StartChat (mypid, (head . tail) ps))
  waitForTerminate
  terminate

waitForTerminate :: Process()
waitForTerminate = do
    Terminate<- expect
    say "The End"
    
waitForPongs :: [ProcessId] -> Process()
waitForPongs [] = return ()
waitForPongs ps = do m <- expect
                     case m of
                       Pong p -> do say $ printf "pong received from %s" (show p)
                                    waitForPongs (filter (/= p) ps)
                       _ -> say "Something else"
-- <<main
main :: IO ()
main = distribMain master Main.__remoteTable
-- >>
