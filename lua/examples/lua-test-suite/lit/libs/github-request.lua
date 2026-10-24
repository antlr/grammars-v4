--[[

Copyright 2014-2015 The Luvit Authors. All Rights Reserved.

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

local env = require('env')
local log = require('log').log
local jsonParse = require('json').parse
local http = require('coro-http')

return function (path, etag)
  local url = "https://api.github.com" .. path
  log("github request", url)

  local headers = {
    {"User-Agent", "lit"},
  }

  -- Set GITHUB_TOKEN to a token from https://github.com/settings/tokens/new to increase the rate limit
  local token = env.get("GITHUB_TOKEN")
  if token then
    headers[#headers + 1] = {"Authorization", "token " .. token}
  end

  if etag then
    headers[#headers + 1] = {"If-None-Match", etag}
  end

  local head, json = http.request("GET", url, headers)

  json = jsonParse(json) or json
  return head, json, url
end

