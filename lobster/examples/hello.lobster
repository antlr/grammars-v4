// A minimal Lobster program: var/let bindings, a function, a for loop,
// and string interpolation.

let name = "World"
var count = 3

def greet(who:string):
    print "Hello, {who}!"

for(count) i:
    greet(name)

print "Done after {count} greetings."
