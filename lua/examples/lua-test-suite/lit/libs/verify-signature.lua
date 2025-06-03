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

local sshRsa = require('ssh-rsa')

return function (db, username, raw)

  local body, fingerprint, signature = string.match(raw, "^(.*)"
    .. "%-%-%-%-%-BEGIN RSA SIGNATURE%-%-%-%-%-\n"
    .. "Format: sha256%-ssh%-rsa\n"
    .. "Fingerprint: ([^\n]+)\n\n"
    .. "(.*)"
    .. "%-%-%-%-%-END RSA SIGNATURE%-%-%-%-%-")

  if not signature then
    return nil, "Missing sha256-ssh-rsa signature"
  end
  signature = signature:gsub("\n", "")
  local iter = db.owners(username)
  local sshKey
  if iter then
    for owner in iter do
      db.importKeys(owner)
      sshKey = db.readKey(owner, fingerprint)
      if sshKey then break end
    end
    if not sshKey then
      return nil, "Not in group: " .. username
    end
  else
    db.importKeys(username)
    sshKey = db.readKey(username, fingerprint)
    if not sshKey then
      return nil, "Invalid fingerprint " .. fingerprint .. " for " .. username
    end
  end
  sshKey = sshRsa.loadPublic(sshKey)
  assert(sshRsa.fingerprint(sshKey) == fingerprint, "fingerprint mismatch")
  return sshRsa.verify(body, signature, sshKey)
end
