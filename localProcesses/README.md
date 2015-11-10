# Local Processes

## Setup
  In order to build and run these examples the user can just use the 'cabal build' command 
  and then run them (from one machine) from the dist/build/NonBlocking/ or dist/build/SpawnLocal
  directories
## SpawnLocal.hs
  In this example a local process is created that will act as a server.
  It is able to receive two types of messages, (ProcessId, String) and String.
  If the (ProcessId, String) message type is received then the replyBack function
  will be called. If a String message type is received then the logMessage function
  will be called. If it receives message of some other type than the above mentioned
  then those messages will be ignored.

  The receiveWait function is used to have the server wait until messages arrive and then
  match the message to the function in order until a match is found. The use of 'forever'
  allows us to receive more than a single message


## NonBlocking.hs
 This example is a simple example of how the expect function is nonblocking. The 'server'
 is expecting messages in the following order: ProcessID, Int(optional), String. The
 'client' is sending the messages in the following Order: Int, String, ProcessId. Messages
 seem to be put in a buffer and fetched when the code is ready for them. 
