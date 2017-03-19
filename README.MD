The main goal of this library is to provide type-safe communication between client and server (in this case an Express webserver), while keeping everything simple and explicit. 

An introductory blogpost for this library can be found here:

http://frigoeu.github.io/phantomtypes.html

This library basically consists of three main parts:

1. data Endpoint
2. execEndpoint
3. hostEndpoint

Check out an example project here: 

https://github.com/FrigoEU/purescript-endpoints-express-example

This library is in a very early phase, a bunch of things could be improved and cleaned up. Everybody's welcome to join and contribute!
* The seperate FileEndpoint / hostFileUploadEndpoint / execFileUploadEndpoint : Probably there is a way to integrate these with the main Endpoint stuff, but I haven't thought about it too much yet.
* Not all HTTP methods are implemented.
* The Blob vs Buffer stuff needs to be cleaned up.

I'm not using purescript-express since it contains a bunch of stuff that I don't really want or need. I just defined some FFI functions that suffice for now.
