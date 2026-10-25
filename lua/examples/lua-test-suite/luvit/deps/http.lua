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

--[[lit-meta
  name = "luvit/http"
  version = "2.1.5"
  dependencies = {
    "luvit/net@2.0.0",
    "luvit/url@2.0.0",
    "luvit/http-codec@2.0.0",
    "luvit/stream@2.0.0",
    "luvit/utils@2.0.0",
    "luvit/http-header@1.0.0",
  }
  license = "Apache 2"
  homepage = "https://github.com/luvit/luvit/blob/master/deps/http.lua"
  description = "Node-style http client and server module for luvit"
  tags = {"luvit", "http", "stream"}
]]

local net = require('net')
local url = require('url')
local codec = require('http-codec')
local Writable = require('stream').Writable
local date = require('os').date
local luvi = require('luvi')
local utils = require('utils')
local httpHeader = require('http-header')

local IncomingMessage = net.Socket:extend()

function IncomingMessage:initialize(head, socket)
  net.Socket.initialize(self)
  self.httpVersion = tostring(head.version)
  self.headers = httpHeader.getHeaders(head)
  if head.method then
    -- server specific
    self.method = head.method
    self.url = head.path
  else
    -- client specific
    self.statusCode = head.code
    self.statusMessage = head.reason
  end
  self.socket = socket
end

function IncomingMessage:_read()
  self.socket:resume()
end

local ServerResponse = Writable:extend()

function ServerResponse:initialize(socket)
  Writable.initialize(self)
  local encode = codec.encoder()
  self.socket = socket
  self.encode = encode
  self.statusCode = 200
  self.headersSent = false
  self.headers = httpHeader.newHeaders()

  local extra = self._extra_http or {}
  self._extra_http = extra
  for _, evt in pairs({'close', 'drain', 'end' }) do
    if extra[evt] then
      self.socket:removeListener(evt,extra[evt])
    end
    extra[evt] = utils.bind(self.emit, self, evt)
    self.socket:on(evt, extra[evt])
  end
end

-- Override this in the instance to not send the date
ServerResponse.sendDate = true

function ServerResponse:setHeader(name, value)
  assert(not self.headersSent, "headers already sent")
  self.headers[name] = value
end

function ServerResponse:getHeader(name)
  assert(not self.headersSent, "headers already sent")
  return self.headers[name]
end

function ServerResponse:removeHeader(name)
  assert(not self.headersSent, "headers already sent")
  self.headers[name] = nil
end

function ServerResponse:flushHeaders()
  if self.headersSent then return end
  self.headersSent = true
  local headers = self.headers
  local statusCode = self.statusCode

  local head = {}
  local sent_date, sent_connection, sent_transfer_encoding, sent_content_length
  for i = 1, #headers do
    local key, value = headers[i][1], headers[i][2]
    local klower = key:lower()
    head[#head + 1] = {tostring(key), tostring(value)}
    if klower == "connection" then
      self.keepAlive = value:lower() ~= "close"
      sent_connection = true
    elseif klower == "transfer-encoding" then
      sent_transfer_encoding = true
    elseif klower == "content-length" then
      sent_content_length = true
    elseif klower == "date" then
      sent_date = true
    end
    head[i] = headers[i]
  end

  if not sent_date and self.sendDate then
    head[#head + 1] = {"Date", date("!%a, %d %b %Y %H:%M:%S GMT")}
  end
  if self.hasBody and not sent_transfer_encoding and not sent_content_length then
    sent_transfer_encoding = true
    head[#head + 1] = {"Transfer-Encoding", "chunked"}
  end
  if not sent_connection then
    if self.keepAlive then
      if self.hasBody then
        if sent_transfer_encoding or sent_content_length then
          head[#head + 1] = {"Connection", "keep-alive"}
        else
          -- body has no length so close to indicate end
          self.keepAlive = false
          head[#head + 1] = {"Connection", "close"}
        end
      elseif statusCode >= 300 then
        self.keepAlive = false
        head[#head + 1] = {"Connection", "close"}
      else
        head[#head + 1] = {"Connection", "keep-alive"}
      end
    else
      self.keepAlive = false
      head[#head + 1] = {"Connection", "close"}
    end
  end
  head.code = statusCode
  local h = self.encode(head)
  self.socket:write(h)
end

function ServerResponse:write(chunk, callback)
  if chunk and #chunk > 0 then
    self.hasBody = true
  end
  self:flushHeaders()
  return self.socket:write(self.encode(chunk), callback)
end

function ServerResponse:_end()
  self:finish()
end

function ServerResponse:finish(chunk)
  if chunk and #chunk > 0 then
    self.hasBody = true
  end
  self:flushHeaders()
  local last = ""
  if chunk then
    last = last .. self.encode(chunk)
  end
  last = last .. (self.encode("") or "")
  local function maybeClose()
    self:emit('finish')
    if not self.keepAlive then
      self.socket:_end()
    end
  end
  if #last > 0 then
    self.socket:write(last, function()
      maybeClose()
    end)
  else
    maybeClose()
  end
end

function ServerResponse:writeHead(newStatusCode, newHeaders)
  assert(not self.headersSent, "headers already sent")
  self.statusCode = newStatusCode
  self.headers = httpHeader.toHeaders(newHeaders)
end

local function handleConnection(socket, onRequest)

  -- Initialize the two halves of the stateful decoder and encoder for HTTP.
  local decode = codec.decoder()

  local buffer = ""
  local req, res

  local function flush()
    req:push()
    req = nil
  end

  local function onTimeout()
    socket:_end()
  end

  local function onEnd()
    -- Just in case the stream ended and we still had an open request,
    -- end it.
    if req then flush() end
  end

  local function onData(chunk)
    -- Run the chunk through the decoder by concatenating and looping
    buffer = buffer .. chunk
    while true do
      local R, event, extra = pcall(decode,buffer)
      if R then
        -- nil extra means the decoder needs more data, we're done here.
        if not extra then break end
        -- Store the leftover data.
        buffer = extra
        if type(event) == "table" then
          -- If there was an old request that never closed, end it.
          if req then flush() end
          -- Create a new request object
          req = IncomingMessage:new(event, socket)
          -- Create a new response object
          res = ServerResponse:new(socket)
          res.keepAlive = event.keepAlive

          -- If the request upgrades the protocol then detatch the listeners so http codec is no longer used
          if req.headers.upgrade then
            req.is_upgraded = true
            socket:setTimeout(0)
            socket:removeListener("timeout", onTimeout)
            socket:removeListener("data", onData)
            socket:removeListener("end", onEnd)
            if #buffer > 0 then
              socket:pause()
              socket:unshift(buffer)
            end
            onRequest(req, res)
            break
          else
            -- Call the user callback to handle the request
            onRequest(req, res)
          end
        elseif req and type(event) == "string" then
          if #event == 0 then
            -- Empty string in http-decoder means end of body
            -- End the request stream and remove the req reference.
            flush()
          else
            -- Forward non-empty body chunks to the req stream.
            if not req:push(event) then
              -- If it's queue is full, pause the source stream
              -- This will be resumed by IncomingMessage:_read
              socket:pause()
            end
          end
        end
      else
        socket:emit('error',event)
        break
      end
    end
  end
  socket:once('timeout', onTimeout)
  -- set socket timeout
  socket:setTimeout(120000)
  socket:on('data', onData)
  socket:on('end', onEnd)
end

local function createServer(onRequest)
  return net.createServer(function (socket)
    return handleConnection(socket, onRequest)
  end)
end

local ClientRequest = Writable:extend()

function ClientRequest.getDefaultUserAgent()
  if ClientRequest._defaultUserAgent == nil then
    ClientRequest._defaultUserAgent = 'luvit/http luvi/' .. luvi.version
  end
  return ClientRequest._defaultUserAgent
end

function ClientRequest:initialize(options, callback)
  Writable.initialize(self)
  self:cork()
  local headers = httpHeader.toHeaders(options.headers)

  local host_found, connection_found, user_agent
  for i = 1, #headers do
    self[#self + 1] = headers[i]
    local key, value = headers[i][1], headers[i][2]
    local klower = key:lower()
    if klower == 'host' then host_found = value end
    if klower == 'connection' then connection_found = value end
    if klower == 'user-agent' then user_agent = value end
  end

  if not user_agent then
    user_agent = self.getDefaultUserAgent()

    if user_agent ~= '' then
      table.insert(self, 1, { 'User-Agent', user_agent })
    end
  end

  options.host = host_found or options.hostname or options.host

  if not host_found and options.host then
    table.insert(self, 1, { 'Host', options.host })
  end

  self.host = options.host
  self.method = (options.method or 'GET'):upper()
  self.path = options.path or '/'
  self.port = options.port or 80
  self.self_sent = false
  self.connection = connection_found

  self.encode = codec.encoder()
  self.decode = codec.decoder()

  if callback then
    self:once('response', callback)
  end

  local buffer = ''
  local res

  local function flush()
    if res then
      res:push()
      res = nil
    end
  end

  local socket = options.socket or net.createConnection(self.port, self.host)
  local connect_emitter = options.connect_emitter or 'connect'

  self.socket = socket
  socket:on('error',function(...) self:emit('error',...) end)
  socket:on(connect_emitter, function()
    self.connected = true
    self:emit('socket', socket)

    local function onData(chunk)
      -- Run the chunk through the decoder by concatenating and looping
      buffer = buffer .. chunk
      while true do
        local R, event, extra = pcall(self.decode,buffer)
        if R==true then
          -- nil extra means the decoder needs more data, we're done here.
          if not extra then return end
          -- Store the leftover data.
          buffer = extra
          if type(event) == "table" then
            if not res then
              flush()
              res = IncomingMessage:new(event, socket)
            end
            if self.method == 'CONNECT' or res.headers.upgrade then
              local evt = self.method == 'CONNECT' and 'connect' or 'upgrade'
              if self:listenerCount(evt) > 0 then
                socket:removeListener('data', onData)
                socket:removeListener('end', flush)
                socket:read(0)
                if #buffer > 0 then
                  socket:pause()
                  socket:unshift(buffer)
                end
                return self:emit(evt, res, socket, event)
              elseif self.method == 'CONNECT' or res.statusCode == 101 then
                return self:destroy()
              end
            end
            self:emit('response', res)
          elseif res and type(event) == "string" then
            if #event == 0 then
              -- Empty string in http-decoder means end of body
              -- End the res stream and remove the res reference.
              flush()
            else
              -- Forward non-empty body chunks to the res stream.
              if not res:push(event) then
                -- If it's queue is full, pause the source stream
                -- This will be resumed by IncomingMessage:_read
                socket:pause()
              end
            end
          end
        else
          return self:emit('error', event)
        end
      end
    end
    socket:on('data', onData)
    socket:on('end', flush)

    if self.ended then
      return self:_done(self.ended.data, self.ended.cb)
    end
  end)
end

function ClientRequest:flushHeaders()
  if not self.headers_sent then
    self.headers_sent = true
    -- set connection
    self:_setConnection()
    Writable.write(self, self.encode(self))
  end
end

function ClientRequest:write(data, cb)
  self:flushHeaders()
  local encoded = self.encode(data)

  -- Don't write empty strings to the socket, it breaks HTTPS.
  if encoded and #encoded > 0 then
    Writable.write(self, encoded, cb)
  else
    if cb then
      cb()
    end
  end
end

function ClientRequest:_write(data, cb)
  self.socket:write(data, cb)
end

function ClientRequest:_done(data, cb)
  self:_end(data, function()
    if cb then
      cb()
    end
  end)
end

function ClientRequest:_setConnection()
  if not self.connection then
    table.insert(self, { 'connection', 'close' })
  end
end

function ClientRequest:done(data, cb)
  -- Optionally send one more chunk
  if data then self:write(data) end

  self:flushHeaders()

  local ended =
    {
      cb = cb or function() end,
      data = ''
    }
  if self.connected then
    self:_done(ended.data, ended.cb)
  else
    self.ended = ended
  end
end

function ClientRequest:setTimeout(msecs, callback)
  if self.socket then
    self.socket:setTimeout(msecs,callback)
  end
end

function ClientRequest:destroy()
  if self.socket then
    self.socket:destroy()
  end
end

local function parseUrl(options)
  if type(options) == 'string' then
    options = url.parse(options)
  end
  return options
end

local function request(options, onResponse)
  return ClientRequest:new(parseUrl(options), onResponse)
end

local function get(options, onResponse)
  options = parseUrl(options)
  options.method = 'GET'
  local req = request(options, onResponse)
  req:done()
  return req
end

return {
  headerMeta = httpHeader.headerMeta, -- for backwards compatibility
  IncomingMessage = IncomingMessage,
  ServerResponse = ServerResponse,
  handleConnection = handleConnection,
  createServer = createServer,
  ClientRequest = ClientRequest,
  parseUrl = parseUrl,
  request = request,
  get = get,
}
