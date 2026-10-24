--[[

Copyright 2014-2016 The Luvit Authors. All Rights Reserved.

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

local uv = require('uv')
local semver = require('semver')
local log = require('log').log
local netConnect = require('coro-net').connect
local httpCodec = require('http-codec')
local websocketCodec = require('websocket-codec')
local makeRemote = require('codec').makeRemote
local deframe = require('git').deframe
local decodeTag = require('git').decoders.tag
local verifySignature = require('verify-signature')

local function connectRemote(url, timeout)
  local protocol, host, port, path = string.match(url, "^(wss?)://([^:/]+):?(%d*)(/?[^#]*)")
  local tls
  if protocol == "ws" then
    port = tonumber(port) or 80
    tls = false
  elseif protocol == "wss" then
    port = tonumber(port) or 443
    tls = true
  else
    error("Sorry, only ws:// or wss:// protocols supported")
  end
  if #path == 0 then path = "/" end

  local read, write, socket, updateDecoder, updateEncoder = assert(netConnect({
    host = host,
    port = port,
    tls = tls,
    encoder = httpCodec.encoder,
    decoder = httpCodec.decoder,
  }, timeout))

  -- Perform the websocket handshake
  assert(websocketCodec.handshake({
    host = host,
    path = path,
    protocol = "lit"
  }, function (req)
    write(req)
    local res = read()
    if not res then error("Missing server response") end
    if res.code == 400 then
      p { req = req, res = res }
      local reason = read() or res.reason
      error("Invalid request: " .. reason)
    end
    return res
  end))

  -- Upgrade the protocol to websocket
  updateDecoder(websocketCodec.decode)
  updateEncoder(websocketCodec.encode)

  return socket, makeRemote(read, write, true)
end

return function(db, url, timeout)

  -- Implement very basic connection keepalive using an uv_idle timer
  -- This will disconnect very quickly if a connect() isn't called
  -- soon after a disconnect()
  local keepalive, remote, socket
  local function connect()
    if remote then
      keepalive:stop()
    else
      log("connecting", url)
      socket, remote = connectRemote(url, timeout)
      keepalive = uv.new_timer()
    end
  end
  local function close()
    if remote then
      -- log("disconnecting", url)
      socket:close()
      keepalive:close()
      keepalive = nil
      remote = nil
      socket = nil
    end
  end
  local function disconnect()
    keepalive:start(1000, 0, close)
  end

  function db.readRemote(author, name, version)
    local tag = author .. "/" .. name
    connect()
    local query = version and (tag .. " " .. version) or tag
    remote.writeAs("read", query)
    local data = remote.readAs("reply")
    disconnect()
    return data
  end

  db.upstream = url

  db.offlineMatch = db.match
  function db.match(author, name, version)
    local match, hash = db.offlineMatch(author, name, version)
    local tag = author .. "/" .. name
    connect()
    local query = version and (tag .. " " .. version) or tag
    remote.writeAs("match", query)
    local data = remote.readAs("reply")
    disconnect()
    local upMatch, upHash
    if data then
      upMatch, upHash = string.match(data, "^([^ ]+) (.*)$")
    end
    if semver.gte(match, upMatch) then
      return match, hash
    end
    return upMatch, upHash
  end

  db.offlineLoad = db.load
  function db.load(hash)
    local raw = db.offlineLoad(hash)
    if raw then return raw end
    db.fetch({hash})
    return assert(db.offlineLoad(hash))
  end

  function db.fetch(list)
    local refs = {}
    repeat
      local hashes = list
      list = {}
      -- Fetch any hashes from list we don't have already
      local wants = {}
      local pending = {}
      for i = 1, #hashes do
        local hash = hashes[i]
        if not pending[hash] and not db.has(hash) then
          wants[#wants + 1] = hash
          pending[hash] = true
        end
      end
      if #wants > 0 then
        connect()
        log("fetching", #wants .. " object" .. (#wants == 1 and "" or "s"))
        remote.writeAs("wants", wants)
        for i = 1, #wants do
          local hash = wants[i]
          local data = remote.readAs("send")
          if data:sub(1, 3) == "tag" then
            local kind, raw = deframe(data)
            assert(kind == "tag")
            local tag = decodeTag(raw)
            if db.config.verifySignatures then
              local owner = tag.tag:match("[^/]+")
              local ok = verifySignature(db, owner, raw)
              if ok then
                log("importing", tag.tag, "highlight")
              else
                log("importing", tag.tag, "failure")
              end
            end
          end
          local actual = db.save(data)
          if actual ~= hash then
            p {
              data = data,
              wants = wants,
              expected = hash,
              actual = actual
            }
            error("Expected hash " .. hash .. " but got " .. actual .. " in result")
          end
        end
        disconnect()
      end

      -- Process the hashes looking for child nodes
      for i = 1, #hashes do
        local hash = hashes[i]
        local kind, value = db.loadAny(hash)
        if kind == "tag" then
          -- TODO: verify tag
          refs[value.tag] = hash
          table.insert(list, value.object)
        elseif kind == "tree" then
          for j = 1, #value do
            local subHash = value[j].hash
            table.insert(list, subHash)
          end
        end
      end
    until #list == 0
    for ref, hash in pairs(refs) do
      local author, name, version = string.match(ref, "^([^/]+)/(.*)/v(.*)$")
      db.write(author, name, version, hash)
    end
    return refs
  end

  function db.push(hash)
    connect()
    remote.writeAs("send", db.load(hash))
    while true do
      local name, data = remote.read()
      if name == "wants" then
        for i = 1, #data do
          remote.writeAs("send", db.load(data[i]))
        end
      elseif name == "done" then
        disconnect()
        return data
      else
        error(name
          and ("Expected 'wants' or 'done', but found " .. name)
          or "Disconnected while waiting for 'wants' or 'done'")
      end
    end
  end

  function db.upquery(name, request)
    connect()
    remote.writeAs(name, request)
    local reply = remote.readAs("reply")
    disconnect()
    return reply
  end

  return db
end
