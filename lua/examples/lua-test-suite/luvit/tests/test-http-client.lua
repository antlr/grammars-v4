--[[

Copyright 2015 The Luvit Authors. All Rights Reserved.

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS-IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.

--]]

require('tap')(function(test)

  local fs = require('fs')
  local http = require('http')

  test("http-client", function(expect)
    http.get('http://github.com', expect(function (res)
      assert(res.statusCode == 301)
      assert(res.httpVersion == '1.1')
      res:on('data', function (chunk)
        p("ondata", {chunk=chunk})
      end)
      res:on('end', expect(function ()
        p('stream ended')
      end))
    end))
  end)

  test("http-client (errors are bubbled)", function(expect)
    local req = http.get('http://unknown123456789.com:1234', function (res)
      assert(false)
    end)
    req:on('error',expect(function(err)
      assert(not (err == nil))
    end))
    req:setTimeout(25000)
  end)

  test("http-client stream file", function(expect)
    local port = 50010

    local function interceptEmit(stream, logString)
      local oldEmit = stream.emit
      stream.emit = function(self, type, ...)
        print(logString .. type)
        return oldEmit(self, type, ...)
      end
    end

    local server
    server = http.createServer(function (req, res)
      local f = fs.createReadStream(module.path)
      interceptEmit(f, 'readable: ')
      interceptEmit(res, 'response: ')
      res:on('close', function() print('response: close(r)') server:destroy() end)
      f:pipe(res)
    end):listen(port, function()
      print('Server running ' .. port)
      http.get('http://127.0.0.1:' .. port, function (res)
        res:on('data', p)
        assert(res.statusCode == 200, 'validate status code')
      end)
    end)
  end)

  test("http-client ignore HTTP/2 upgrade from server", function(expect)
    local port = 50020

    local server
    server = http.createServer(expect(function(req, res)
      print('server request', req.method)
      assert(req.method == 'GET')
      assert(req.url == 'test_path')
      -- advertise HTTP/2 support
      res:setHeader('Upgrade', 'h2,h2c')
      return res:finish('response')
    end))

    server:listen(port, expect(function()
      print('server running')
      local options = {
        host = '127.0.0.1',
        port = port,
        path = 'test_path',
      }

      local req = http.request(options, function(res)
        print('req cb')
        assert(res.headers.upgrade == 'h2,h2c')
        res:on('data', function(data)
          print('req data')
          assert(data == 'response')
          server:destroy()
        end)
      end)

      req:setTimeout(10*1e3, function()
        error('req timeout')
      end)

      req:done()
    end))
  end)

  test("http-client accept Websocket upgrade from server", function(expect)
    local port = 50030

    local server
    server = http.createServer(expect(function(req, res)
      print('server request', req.method)
      assert(req.method == 'GET')
      assert(req.url == 'test_path')
      assert(req.headers.upgrade == 'websocket')
      res:writeHead(101, {
        ['Upgrade'] = 'websocket',
        ['Connection'] = 'Upgrade',
        -- other ws headers
      })
      res:flushHeaders()

      res.socket:on('data', function(data)
        print('response data')
        assert(data == 'request')
        res.socket:destroy()
        server:destroy()
      end)

      res.socket:write('response')
    end))

    server:listen(port, expect(function()
      print('server running')
      local options = {
        host = '127.0.0.1',
        port = port,
        path = 'test_path',
        headers = {
          ['Upgrade'] = 'websocket',
          ['Connection'] = 'Upgrade',
          -- other ws headers
        }
      }

      local req = http.request(options, function(res)
        error('response callback should not be called')
      end)

      req:once('upgrade', expect(function(res, socket, event)
        print('req upgrade')
        assert(res.headers.upgrade == 'websocket')
        socket:resume()
        socket:write('request')
      end))

      req:setTimeout(10*1e3, function()
        error('req timeout')
      end)

      req:done()
    end))
  end)

  test("http-client connect", function(expect)
    local port = 50040

    local server
    server = http.createServer(expect(function(req, res)
      print('server request', req.method)
      assert(req.method == 'CONNECT')
      assert(req.url == 'test_path')
      -- TODO: CONNECT should be detached from http codec.
      return res:finish('response')
    end))

    server:listen(port, expect(function()
      print('server running')
      local options = {
        host = '127.0.0.1',
        port = port,
        method = 'CONNECT',
        path = 'test_path',
      }

      local req = http.request(options, function(res)
        error('response callback should not be called')
      end)

      req:once('connect', expect(function(res, socket, event)
        print('req connect')
        socket:on('data', function(data)
          print('socket data')
          assert(data)
          assert(data:find('response'))
          server:destroy()
        end)

        socket:resume()
      end))

      req:setTimeout(10*1e3, function()
        error('req timeout')
      end)

      req:done()
    end))
  end)

end)
