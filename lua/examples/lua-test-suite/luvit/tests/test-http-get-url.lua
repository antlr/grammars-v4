local url = require("url")
local http = require('http')
local PORT = process.env.PORT or 10083

require('tap')(function(test)

  test('http-get-url', function(expect)
    local seen_req = false
    local server
    server = http.createServer(function(req, res)
      assert('GET' == req.method)
      assert('/foo?bar' == req.url)
      res:writeHead(200, {['Content-Type'] = 'text/plain'})
      res:write('hello\n')
      res:finish()
      server:close()
      seen_req = true
    end)

    server:listen(PORT, expect(function()
      http.get('http://127.0.0.1:' .. PORT .. '/foo?bar');
    end))
  end)

end)
