#Remote Processes
##Setup
After using the 'cabal build' command the user can run the master and slave on multiple machines or on a single machine.
If running on the same machine the dist/build/ping-multi/ping-multi binary can be run without port or hostname arguments,
localhost is the default host and 44444 is the default port, it is required to specify if the process is a master or slave.

The ping-multi.hs example requires two slaves and one master. The slaves should be run before running the master. In order
to run the slave programs remotely the user will need to put in the IP address that others will be using to communicate with it.
If you are running the slaves on the same machine the port numbers will have to be different.

## ping-multi

This example has the master asking one of the slaves to perform the fibonacci of a user input number, and then asks the other
server the factorial of a number. If there are more than 2 slaves than the master program will only be communicating with the first
two programs. The Master then tells one fo the slaves to talk to the other, this can be looked at a master telling a slave to run 
a certain protocol with another entity. 
