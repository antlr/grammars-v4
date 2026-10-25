local http = require('http')


local req
req = http.request('https://luvit.io/logo-white.svg', function(res)
  p("on_connect", {statusCode = res.statusCode, headers = res.headers})
  res:on('data', function (chunk)
    p("on_data", #chunk)
  end)
  res:on("end", function ()
    p("on_end")
  end)
end)

req:done()
