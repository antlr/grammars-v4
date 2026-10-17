--[[

Copyright 2014 The Luvit Authors. All Rights Reserved.

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

local childprocess = require('childprocess')
local fs = require('fs')
local los = require('los')
local path = require('luvi').path
local table = require('table')
local timer = require('timer')
local tls = require('tls')

if los.type() == 'win32' then
  return
end

local function loadPEM(rootName)
  return path.join(module.dir, 'fixtures', 'keys', rootName)
end

require('tap')(function(test)
  test('set null cipher', function(expect)
    local options, reply, response, server
    local port, onConnection, onListen

    port = 32000

    options = {
      cert = fs.readFileSync(loadPEM('agent1-cert.pem')),
      key = fs.readFileSync(loadPEM('agent1-key.pem')),
      port = port,
      rejectUnauthorized = false,
      ciphers = 'NULL-MD5'
    }

    reply = 'I AM THE WALRUS\n'
    response = ''

    function onConnection(conn)
      conn:write(reply, function() conn:destroy() end)
    end

    function onListen()
      local args, child, onExit, onStdout

      args = {
        's_client',
        '-tls1',
        '-cipher', 'NULL-MD5',
        '-connect', '127.0.0.1:' .. port
      }

      function onStdout(data)
        response = response .. data
      end

      function onExit()
        p(response)
        assert(response:find(reply) ~= -1)
        server:close()
      end

      child = childprocess.spawn('openssl', args)
      child:on('exit', onExit)
      child.stdout:on('data', onStdout)
    end

    server = tls.createServer(options, expect(onConnection))
    server:listen(port, '127.0.0.1', expect(onListen))
  end)
end)
