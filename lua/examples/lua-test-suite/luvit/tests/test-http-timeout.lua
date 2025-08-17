local url = require("url")
local http = require('http')

require('tap')(function(test)

  test('http-timeout', function(expect)
    local PORT = process.env.PORT or 10086
    local options = {
      method = 'GET',
      port = PORT,
      host = '127.0.0.1',
      path = '/'
    }

    local server
    server = http.createServer(function(req, res)end)

    server:listen(PORT, function()
      local req = http.request(options, function(res)
      end)
      function destroy()
        server:close()
        req:destroy()
      end
      req:setTimeout(1, destroy)
      req:on('error', function(err)
        assert(err.code == "ECONNRESET")
      end)
    end)
  end)
end)