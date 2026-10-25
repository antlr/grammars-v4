local http = require('http')

local options = {
  host = "luvit.io",
  port = 80,
  path = "/"
}

local req = http.request(options, function (res)
  res:on('data', function (chunk)
    p("ondata", {chunk=chunk})
  end)
end)
req:done()
