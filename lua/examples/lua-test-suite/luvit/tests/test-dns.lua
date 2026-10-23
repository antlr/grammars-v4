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

local dns = require('dns')
local los = require('los')
local path = require('luvi').path

-- Appveyor is failing builds randomly... need to re-enable
if require('os').getenv('APPVEYOR') then return end

require('tap')(function (test)
  test("resolve4", function (expect)
    dns.resolve4('luvit.io', expect(function(err, answers)
      assert(not err)
      assert(#answers > 0)
      p(answers)
    end))
  end)
  test("resolve4ip", function (expect)
    dns.resolve4('127.0.0.1', expect(function(err, answers)
      assert(not err)
      assert(#answers > 0)
      p(answers)
    end))
  end)
  test("resolve6", function (expect)
    dns.resolve6('google.com', expect(function(err, answers)
      assert(not err)
      p(answers)
      assert(#answers > 0)
    end))
  end)
  test("resolve6ip", function (expect)
    dns.resolve6('::1', expect(function(err, answers)
      assert(not err)
      p(answers)
      assert(#answers > 0)
    end))
  end)
  test("resolveSrv", function (expect)
    dns.resolveSrv('www.luvit.io', expect(function(err, answers)
      assert(not err)
      p(answers)
      assert(#answers > 0)
    end))
  end)
  test("resolveMx", function (expect)
    dns.resolveMx('google.com', expect(function(err, answers)
      assert(not err)
      p(answers)
      assert(#answers > 0)
    end))
  end)
  test("resolveNs", function (expect)
    dns.resolveNs('google.com', expect(function(err, answers)
      assert(not err)
      p(answers)
      assert(#answers > 0)
    end))
  end)
  test("resolveCname", function (expect)
    dns.setTimeout(5000)
    dns.resolveCname('www.luvit.io', expect(function(err, answers)
      assert(not err)
      p(answers)
      assert(#answers > 0)
    end))
  end)
  test("resolveTxt", function (expect)
    -- for some reason this test was failing on linux using the system resolver, so
    -- use the defined default servers instead (the servers will change on the
    -- next test anyway so this only affects this test)
    dns.setDefaultServers()
    dns.resolveTxt('google.com', expect(function(err, answers)
      assert(not err, err)
      p(answers)
      assert(#answers > 0)
    end))
  end)
  test("resolveTxtTimeout Order", function (expect)
    dns.setServers( { { ['host'] = '127.0.0.1', ['port'] = 53234 }, { ['host'] = '8.8.8.8', ['port'] = 53 } })
    dns.setTimeout(2000)
    dns.resolveTxt('google.com', expect(function(err, answers)
      assert(not err)
      p(answers)
      assert(#answers > 0)
      assert(answers[1].server.host == '8.8.8.8')
    end))
  end)
  test("resolveTxtTimeout", function (expect)
    dns.setServers( { { ['host'] = '127.0.0.1', ['port'] = 53234 } } )
    dns.setTimeout(200)
    dns.resolveTxt('lit.luvit.io', expect(function(err, answers)
      assert(err)
    end))
  end)
  test("resolveTxtTCP", function (expect)
    if require('os').getenv('TRAVIS') then return end
    dns.setTimeout(2000)
    dns.setServers( { { ['host'] = '8.8.8.8', ['port'] = 53, ['tcp'] = true } } )
    dns.resolveTxt('lit.luvit.io', expect(function(err, answers)
      assert(not err)
    end))
  end)
  test("load resolver", function ()
    if los.type() == 'win32' then return end
    local servers = dns.loadResolver({ file = path.join(module.dir, 'fixtures', 'resolve.conf.a')})
    assert(#servers == 3)
    assert(servers[1].host == '192.168.0.1')
    assert(servers[2].host == '::1')
    assert(servers[3].host == '::2')
    dns.setDefaultServers()
  end)
  test("load resolver (resolv.conf search)", function ()
    if los.type() == 'win32' then return end
    local servers = dns.loadResolver({ file = path.join(module.dir, 'fixtures', 'resolve.conf.b')})
    assert(#servers == 2)
    assert(servers[1].host == '127.0.0.1')
    assert(servers[2].host == '127.0.0.2')
    dns.setDefaultServers()
  end)
  test('bad address', function(expect)
    dns.resolve4('luvit.not_a_domain', expect(function(err)
      assert(err)
      assert(err.code > 0)
    end))
  end)
end)
