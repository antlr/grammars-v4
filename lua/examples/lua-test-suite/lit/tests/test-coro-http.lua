local http = require 'coro-http'

require('tap')(function (test)

  test("real http request", function ()
    coroutine.wrap(function()
      local res, data = http.request('GET', 'http://github.com/')
      assert(res and (res.code == 301 or res.code == 200))
      assert(data)
      local connection = http.getConnection('github.com', 80, false)
      assert(connection)
      connection.socket:close()
    end)()
  end)

  test("real https request", function ()
    coroutine.wrap(function()
      local res, data = http.request('GET', 'https://github.com/')
      assert(res and res.code == 200)
      assert(data)
      local connection = http.getConnection('github.com', 443, true)
      assert(connection)
      connection.socket:close()
    end)()
  end)

end)
