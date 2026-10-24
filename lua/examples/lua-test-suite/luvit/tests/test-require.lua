--[[

Copyright 2012 The Luvit Authors. All Rights Reserved.

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


require('tap')(function (test)

  local makeRequire = require('require')
  local pathJoin = require('luvi').path.join
  local p = require('utils').prettyPrint
  local base = pathJoin(module.dir, "fixtures/fake.lua")
  _G.num_loaded = 0

  test("native lua require should still be there", function ()
    p(require, _G.require)
    assert(require ~= _G.require)
  end)

  test("relative require with extension", function ()
    local require = makeRequire(base)
    local mod1 = require('./libs/module1/init.lua')
    assert(_G.num_loaded == 1)
    assert(mod1[1] == "module1")
  end)

  test("relative require as package with index", function ()
    local require = makeRequire(base)
    local mod1 = require('./libs/module1')
    p(_G.num_loaded)
    assert(_G.num_loaded == 1)
    assert(mod1[1] == "module1")
  end)

  test("relative require with auto-extension", function ()
    local require = makeRequire(base)
    local mod1 = require('./libs/module1/init')
    assert(_G.num_loaded == 1)
    assert(mod1[1] == "module1")
  end)

  test("cached with different paths", function ()
    local require = makeRequire(base)
    local mod1_1 = require('./libs/module1/init.lua')
    local mod1_2 = require('./libs/module1/init')
    local mod1_3 = require('./libs/module1')
    assert(_G.num_loaded == 1)
    assert(mod1_1 == mod1_2)
    assert(mod1_2 == mod1_3)
  end)

  test("Lots-o-requires", function ()
    local require = makeRequire(base)
    local m1 = require("module1")
    local m1_m2 = require("module1/module2")
    local m2_m2 = require("module2/module2")
    local m3 = require("module3")
    local rm1 = require("./libs/module1")
    local rm1_m2 = require("./libs/module1/module2")
    local rm2_m2 = require("./libs/module2/module2")
    local rm3 = require('./libs/module3')
    local rm2sm1_m2 = require("./libs/module2/../module1/module2")
    local rm1sm2_m2 = require("./libs/module1/../module2/module2")
    assert(_G.num_loaded == 4)
    assert(m1 == rm1)
    assert(m1_m2 == rm1_m2 and m1_m2 == rm2sm1_m2)
    assert(m2_m2 == rm2_m2 and m2_m2 == rm1sm2_m2)
    assert(m3 == rm3)
  end)

  test("inter-dependencies", function ()
    local require = makeRequire(base)
    local callbacks = {}
    _G.onexit = function (fn)
      callbacks[#callbacks + 1] = fn
    end
    local e = require('./a')
    p(e)
    p{
      A=e.A(),
      C=e.C(),
      D=e.D(),
    }
    assert(e.A() == 'A')
    assert(e.C() == 'C')
    assert(e.D() == 'D')
    for i = 1, #callbacks do
      callbacks[i]()
    end
    p(e)
    p{
      A=e.A(),
      C=e.C(),
      D=e.D(),
    }
    assert(e.A() == 'A done')
    assert(e.C() == 'C done')
    assert(e.D() == 'D done')
  end)

  test("circular dependencies", function ()
    local require = makeRequire(base)
    local parent = require('parent');
    p(parent)
    assert(parent.child.parent == parent)
    local child = require('child');
    p(child)
    assert(child.parent.child == child)
  end)

  test("deps folder", function ()
    local require = makeRequire(base)
    _G.num_loaded = 0
    local N = require('moduleN')
    p(N, _G.num_loaded)
    assert(_G.num_loaded == 2)
    assert(N[1] == 'moduleN')
    assert(N.M[1] == 'moduleM')
  end)

end)


-- -- Test native addons
-- --[[
-- local vectors = {
--   require("vector"),
--   require("vector-renamed"),
-- }
-- assert(vectors[1] == vectors[2], "Symlinks should realpath and load real module and reuse cache")
-- ]]
