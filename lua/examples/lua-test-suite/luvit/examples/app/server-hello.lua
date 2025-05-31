local server = require("http").createServer(require('stack').stack(
  function (req, res)
    res:writeHead(200, {
      ["Content-Type"] = "text/plain",
      ["Content-Length"] = 12
    })
    res:finish("Hello World\n")
  end
)):listen(8080, "127.0.0.1")

local address = server:address()
p(address)
p("http server listening on http://127.0.0.1:8080/")
