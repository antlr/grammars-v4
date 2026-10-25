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
]]--

local childprocess = require('childprocess')
local los = require('los')
local fs = require('fs')
local path = require('luvi').path
local table = require('table')
local tls = require('tls')
local utils = require('utils')

if require('los').type() == 'win32' then
  return
end

--[[
 This is a rather complex test which sets up various TLS servers with node
 and connects to them using the 'openssl s_client' command line utility
 with various keys. Depending on the certificate authority and other
 parameters given to the server, the various clients are
 - rejected,
 - accepted and "unauthorized", or
 - accepted and "authorized".
]]--

local function loadPEM(rootName)
  return path.join(module.dir, 'fixtures', 'keys', rootName)
end

local function optionsIterator(options)
  local i = 0
  return function()
    i = i + 1
    return options[i]
  end
end

local function runClient(options, port, callback)
  local args = { 's_client', '-connect', '127.0.0.1:' .. port }
  print('  connecting with ' .. options.name)

  if options.name == 'agent1' then
    -- signed by CA1
    table.insert(args, '-key')
    table.insert(args, loadPEM('agent1-key.pem'))
    table.insert(args, '-cert')
    table.insert(args, loadPEM('agent1-cert.pem'))
  elseif options.name == 'agent2' then
    -- self-signed
    table.insert(args, '-key')
    table.insert(args, loadPEM('agent2-key.pem'))
    table.insert(args, '-cert')
    table.insert(args, loadPEM('agent2-cert.pem'))
  elseif options.name == 'agent3' then
    -- signed by CA2
    table.insert(args, '-key')
    table.insert(args, loadPEM('agent3-key.pem'))
    table.insert(args, '-cert')
    table.insert(args, loadPEM('agent3-cert.pem'))
  elseif options.name == 'agent4' then
    table.insert(args, '-key')
    table.insert(args, loadPEM('agent4-key.pem'))
    table.insert(args, '-cert')
    table.insert(args, loadPEM('agent4-cert.pem'))
  elseif options.name == 'nocert' then
  else
    error('Unknown agent name')
  end

  local rejected = true
  local authed = false
  local app = 'openssl'
  local out = ''
  if los.type() == 'win32' then
    app = 'C:\\Program Files\\OpenSSL\\bin\\openssl.exe'
  end
  p(app .. table.concat(args, ' '))
  local child = childprocess.spawn(app, args)
  child.stdout:on('data', function(chunk)
    out = out .. chunk
    if out:find('_unauthed') then
      print('  * unauthed')
      authed = false
      rejected = false
      child.stdin:write('goodbye\n')
    end
    if out:find('_authed') then
      print('  * authed')
      authed = true
      rejected = false
      child.stdin:write('goodbye\n')
    end
  end)
  child:on('exit', function(exit_status, term_signal)
    if options.shouldReject == true then
      assert(rejected == true, options.name .. ' NOT rejected, but should have been')
    else
      assert(rejected == false, options.name .. ' rejected, but should NOT have been')
      assert(options.shouldAuth == authed)
    end
    callback()
  end)
end

local function theTest(options)
  local server, serverKey, serverCert, serverCA, serverCRL, port
  local option, testIter

  port = options.port

  serverKey = fs.readFileSync(
    path.join(module.dir, 'fixtures', 'keys', options.serverKey)
  )
  serverCert = fs.readFileSync(
    path.join(module.dir, 'fixtures', 'keys', options.serverCert)
  )
  if options.serverCRL then
    serverCRL = fs.readFileSync(
      path.join(module.dir, 'fixtures', 'keys', options.serverCRL)
    )
  end
  if type(options.serverCA) == 'table' then
    serverCA = {}
    for _, filename in ipairs(options.serverCA) do
      local path = path.join(module.dir, 'fixtures', 'keys', filename)
      local data = fs.readFileSync(path)
      table.insert(serverCA, data)
    end
  else
    serverCA = fs.readFileSync(
      path.join(module.dir, 'fixtures', 'keys', options.serverCA)
    )
  end

  serverOptions = {
    key = serverKey,
    cert = serverCert,
    ca = serverCA,
    crl = serverCRL,
    requestCert = options.requestCert,
    rejectUnauthorized = options.rejectUnauthorized,
    secureProtocol = tls.DEFAULT_SECUREPROTOCOL
  }

  option = optionsIterator(options.clients)

  function testIter()
    local opt, cli = option()
    if not opt then server:close() return end
    runClient(opt, port, function() testIter() end)
  end

  server = tls.createServer(serverOptions, function(c)
    if c.authorized == true then
      print('- authed connection: ' .. tostring(c:getPeerCertificate():subject()))
      c:write('\n_authed\n')
    else
      print('- unauthed connection: ' .. (c.authorizationError and c.authorizationError.error_string or 'undefined'))
      c:write('\n_unauthed\n')
    end
    c:on('data', function(chunk)
      if chunk:find('goodbye') then
        c:destroy()
      end
    end)
  end)

  server:listen(port, function() testIter() end)
end

require('tap')(function(test)
  test('Do not request certs. Everyone is unauthorized.', function()
    local options = {
      port = 32000,
      serverKey = 'agent2-key.pem',
      serverCert = 'agent2-cert.pem',
      serverCA = 'ca1-cert.pem',
      requestCert = false,
      rejectUnauthorized = false,
      clients = {
        { name = 'agent1', shouldReject = false, shouldAuth = false },
        { name = 'agent2', shouldReject = false, shouldAuth = false },
        { name = 'agent3', shouldReject = false, shouldAuth = false },
        { name = 'nocert', shouldReject = false, shouldAuth = false }
      }
    }
    theTest(options)
  end)

  test('Allow both authed and unauthed connections with CA1', function()
    local options = {
      port = 33000,
      serverKey = 'agent2-key.pem',
      serverCert = 'agent2-cert.pem',
      serverCA = 'ca1-cert.pem',
      requestCert = true,
      rejectUnauthorized = false,
      clients = {
        { name = 'agent1', shouldReject = false, shouldAuth = true },
        { name = 'agent2', shouldReject = false, shouldAuth = false },
        { name = 'agent3', shouldReject = false, shouldAuth = false },
        { name = 'nocert', shouldReject = false, shouldAuth = false }
      }
    }
    theTest(options)
  end)

  test('Allow only authed connections with CA1', function()
    local options = {
      port = 33000,
      serverKey = 'agent2-key.pem',
      serverCert = 'agent2-cert.pem',
      serverCA = 'ca1-cert.pem',
      requestCert = true,
      rejectUnauthorized = true,
      clients = {
        { name = 'agent1', shouldReject = false, shouldAuth = true },
        { name = 'agent2', shouldReject = true },
        { name = 'agent3', shouldReject = true },
        { name = 'nocert', shouldReject = true }
      }
    }
    theTest(options)
  end)

  test('Allow only authed connections with CA1 and CA2', function()
    local options = {
      port = 33000,
      serverKey = 'agent2-key.pem',
      serverCert = 'agent2-cert.pem',
      serverCA = { 'ca1-cert.pem', 'ca2-cert.pem' },
      requestCert = true,
      rejectUnauthorized = true,
      clients = {
        { name = 'agent1', shouldReject = false, shouldAuth = true },
        { name = 'agent2', shouldReject = true },
        { name = 'agent3', shouldReject = false, shouldAuth = true },
        { name = 'nocert', shouldReject = true }
      }
    }
    theTest(options)
  end)
end)

--[[ Add CRL support to lua-openssl
  test('Allow only certs signed by CA2 but not in the CRL', function()
    local options = {
      port = 33000,
      serverKey = 'agent2-key.pem',
      serverCert = 'agent2-cert.pem',
      serverCA = { 'ca2-cert.pem' },
      crl = {'ca2-crl'},
      requestCert = true,
      rejectUnauthorized = true,
      clients = {
        { name = 'agent1', shouldReject = true, shouldAuth = false },
        { name = 'agent2', shouldReject = true, shouldAuth = false },
        { name = 'agent3', shouldReject = false, shouldAuth = true },
        { name = 'agent4', shouldReject = true, shouldAuth = false },
        { name = 'nocert', shouldReject = true }
      }
    }
    theTest(options)
  end)
--]]
