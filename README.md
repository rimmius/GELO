#GELO

JavaScript-ish syntax flavor running on the Erlang VM.

INSTALLATION:<br>
```
git clone git@github.com:gustehn/GELO.git
cd GELO
./rebar compile
```
<br>
Webserver up and spinning in 8 lines of code:<br>
example.gelo
```
function webServer(){
    var port = 1337;
    server.create( port
                   , function(request, socket){
                         console.log(request);
                         server.send(socket, 200, "Content-Type: text/html", "Hello World");
                  });
}
```
Compile it:
```
$ ./gelo compile example
Compilation complete: example
```
And run it:
```
./gelo example webServer
```
Look into example.gelo for more examples!

Happy hacking

//
@gustehn
