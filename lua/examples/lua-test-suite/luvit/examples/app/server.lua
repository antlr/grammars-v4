local HTTP = require "http"
local Stack = require "stack"

local MethodEcho = require "method_echo"
local UrlEcho = require "url_echo"

HTTP.createServer(Stack.stack(
  Stack.mount("/methods",
    MethodEcho("PUT"),
    MethodEcho("GET")
  ),
  Stack.mount("/echo", UrlEcho),
  MethodEcho("DELETE")
)):listen(8080)

print("Server listening at http://localhost:8080/")
