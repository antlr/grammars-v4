local http = require("http")
local https = require("https")
local pathJoin = require('luvi').path.join
local fs = require('fs')

-- A simple keepalive benchmark to see how fast we can make http
-- test with one of the following tools:
--   luvit examples/http-bench.lua
--   ab -t 10 -n 500000 -c 100 -k http://127.0.0.1:8080/
--   httperf --hog 127.0.0.1 --port 8080 --num-conns=100  --num-calls=1000

local function onRequest(req, res)
  local body = "Hello world\n"
  res:setHeader("Content-Type", "text/plain")
  res:setHeader("Content-Length", #body)
  res:finish(body)
end

http.createServer(onRequest):listen(8080)
print("Server listening at http://localhost:8080/")

https.createServer({
  key = fs.readFileSync(pathJoin(module.dir, "key.pem")),
  cert = fs.readFileSync(pathJoin(module.dir, "cert.pem")),
}, onRequest):listen(8443)
print("Server listening at https://localhost:8443/")

