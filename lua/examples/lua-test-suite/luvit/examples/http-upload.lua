local http = require("http")
local utils = require("utils")
local table = require("table")

http.createServer(function (req, res)
  p("on_request", req)
  local chunks = {}
  local length = 0
  req:on('data', function (chunk, len)
    p("on_data", {len=len})
    length = length + 1
    chunks[length] = chunk
  end)
  req:on('end', function ()
    local body = table.concat(chunks, "")
    p("on_end", {total_len=#body})
    body = "length = " .. tostring(#body) .. "\n"
    res:writeHead(200, {
      ["Content-Type"] = "text/plain",
      ["Content-Length"] = #body
    })
    res:finish(body)
  end)

end):listen(8080)

print("Server listening at http://localhost:8080/")

