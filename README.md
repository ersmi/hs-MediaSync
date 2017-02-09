# hs-MediaSync

hs-MediaSync is a simple tool used to keep two folders across a network identical using websockets.

-----
#### Setup
`config.cfg` contains the configuration for the folders to be monitored by the server and client. The client's `hostname` should point to the public or local IP of the server. Ports assigned to each service should be matching.
#### Use
To start the server through Haskell stack on Windows:
```
stack ghc .\server.hs; .\server.exe
```
or linux:
```
ghc ./server.hs; ./server
```

The client has equivalents:
```
stack ghc .\client.hs; .\client.exe
ghc ./client.hs; ./client
```

Alternatively, supplying the `-d` flag to the client will start a thread to request pulls every 10 seconds. Otherwise only a user interface will start.
Note that the server should be running prior to starting the client.

##### Todo
- Automated Testing
- Server-side event monitoring
- Server-side CLI implementation
- Smarter Client-side event monitoring
- File metadata monitoring
- File chunking
