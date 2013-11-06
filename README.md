OAK Test
========

1. Running CLIENT
-----------------
The client was written in Haskell. In order to run it, you need first to install the [Haskell Platform](http://www.haskell.org/platform/) and then follow the steps below.

* Disclaimer: I just liked the challenge of "brushing bits" in Haskell. I thought that it could be interesting and, indeed, it was. :-)

1.2. Directly
-------------
* Run "runhaskell Main.hs \[serverAddress\] \[servicePortNumber\]".

1.3. Compiling
--------------
* Run "make";
* Execute "./client" passing as argument the ip address or hostname of the remote server and the service port number.

1.4. Via GHCi
-------------
* Start GHCi;
* Load file: ":l Main.hs";
* Set arguments: ":set args \[serverAddress\] \[servicePortNumber\]" 
* Type: "main".

2. Running SERVER 
-----------------
As I couldn't get connected to OAK's remote server, I created my own (extremely simple) server to simulate the interaction with the client. That was needed in order to test my application. The server was written using the Java language and it listens to port 9999 by default.

2.1. Directly
-------------
* Compile "Server.java" class using "javac";
* Run "Server".

2.1. Maven
----------
* Ensure you have Maven installed (type: "mvn --version" in the console);
* Move to directory "OAKTestServer";
* Run "mvn clean package";
* Run "mvn exec:java".

