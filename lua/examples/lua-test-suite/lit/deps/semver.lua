--[[lit-meta
  name = "luvit/semver"
  version = "2.0.0"
  homepage = "https://github.com/luvit/lit/blob/master/deps/prompt.lua"
  description = "Parser, comparer and matcher for semantic versions strings."
  tags = {"semver"}
  license = "MIT"
  author = { name = "Tim Caswell" }
]]

local exports = {}
local parse, normalize, match
-- Make the module itself callable
setmetatable(exports, {
  __call = function (_, ...)
    return match(...)
  end
})

function parse(version)
  if not version then return end
  if not tonumber(string.match(version, "^v?(%d+)")) then
    error("Invalid version value: " .. version)
  end
  return
    tonumber(string.match(version, "^v?(%d+)")),
    tonumber(string.match(version, "^v?%d+%.(%d+)") or 0),
    tonumber(string.match(version, "^v?%d+%.%d+%.(%d+)") or 0),
    tonumber(string.match(version, "^v?%d+%.%d+%.%d+-(%d+)") or 0)
end
exports.parse = parse

function normalize(version)
  if not version then return "*" end
  local a, b, c, d = parse(version)
  return a .. '.' .. b .. '.' .. c .. (d > 0 and ('-' .. d) or (''))
end
exports.normalize = normalize

-- Return true is first is greater than or equal to the second
-- nil counts as lowest value in this case
function exports.gte(first, second)
  if not second or first == second then return true end
  if not first then return false end
  local a, b, c, x = parse(second)
  local d, e, f, y = parse(first)
  return (d > a) or (d == a and (e > b or (e == b and (f > c or (f == c and y >= x)))))
end

-- Sanity check for gte code
assert(exports.gte(nil, nil))
assert(exports.gte("0.0.0", nil))
assert(exports.gte("9.9.9", "9.9.9"))
assert(exports.gte("1.2.3", "1.2.3-0"))
assert(exports.gte("1.2.3-4", "1.2.3-4"))
assert(exports.gte("9.9.10", "9.9.9"))
assert(exports.gte("9.10.0", "9.9.99"))
assert(exports.gte("10.0.0", "9.99.99"))
assert(exports.gte("10.0.0-1", "10.0.0-0"))
assert(exports.gte("10.0.1-0", "10.0.0-0"))
assert(exports.gte("10.0.1", "10.0.0-10"))
assert(not exports.gte(nil, "0.0.0"))
assert(not exports.gte("9.9.9", "9.9.10"))
assert(not exports.gte("9.9.99", "9.10.0"))
assert(not exports.gte("9.99.99", "10.0.0"))
assert(not exports.gte("10.0.0-0", "10.0.0-1"))

-- Given a semver string in the format a.b.c, and a list of versions in the
-- same format, return the newest version that is compatable.
-- For all versions, don't match anything older than minumum.
-- For 0.0.z-b, only match build updates
-- For 0.y.z-b, match patch and build updates
-- For x.y.z-b, match minor, patch, and build updates
-- If there is no minumum, grab the absolute maximum.
function match(version, iterator)
  --           Major Minor Patch Build
  -- found     a     b     c     x
  -- possible  d     e     f     y
  -- minimum   g     h     i     z
  local a, b, c, x
  if not version then
    -- With an empty match, simply grab the newest version
    for possible in iterator do
      local d, e, f, y = parse(possible)
      if (not a) or (d > a) or (d == a and (e > b or (e == b and (f > c or (f == c and y > x))))) then
        a, b, c, x = d, e, f, y
      end
    end
  else
    local g, h, i, z = parse(version)
    if g > 0 then
      -- From 1.0.0 and onward, minor updates are allowed since they mean non-
      -- breaking changes or additons.
      for possible in iterator do
        local d, e, f, y = parse(possible)
        -- Must be gte the minimum, but match major version. If this is the
        -- first match, keep it, otherwise, make sure it's better than the last
        -- match.
        if d == g and (e > h or (e == h and (f > i or (f == i and y >= z)))) and
           ((not a) or e > b or (e == b and (f > c or (f == c and y > x)))) then
          a, b, c, x = d, e, f, y
        end
      end
    elseif h > 0 then
      -- Before 1.0.0 we only allow patch updates assuming less stability at
      -- this period.
      for possible in iterator do
        local d, e, f, y = parse(possible)
        -- Must be gte the minumum, but match major and minor versions.
        if d == g and e == h and (f > i or (f == i and y >= z)) and
                      ((not a) or f > c or (f == c and y > x)) then
          a, b, c, x = d, e, f, y
        end
      end
    else
      -- Before 0.1.0 we assume even less stability and only update new builds
      for possible in iterator do
        local d, e, f, y = parse(possible)
        -- Must match major, minor, and patch, only allow build updates
        if d == g and e == h and f == i and y >= z and
                                ((not a) or y > x) then
          a, b, c, x = d, e, f, y
        end
      end
    end
  end
  return a and (a .. '.' .. b .. '.' .. c .. (x > 0 and ('-' .. x) or ('')))
end
exports.match = match

local function iterator()
  local versions = {
    "0.0.1",
    "0.0.2",
    "0.0.3", "0.0.3-1", "0.0.3-2",
    "0.0.4",
    "0.1.0", "0.1.1",
    "0.2.0", "0.2.1",
    "0.3.0", "0.3.0-1", "0.3.0-2", "0.3.1",
    "0.4.0", "0.4.0-1", "0.4.0-2",
    "1.0.0", "1.1.0", "1.1.3",
    "2.0.0", "2.1.2",
    "3.1.4",
    "4.0.0", "4.0.0-1", "4.0.0-2", "4.0.1",
    "5.0.0", "5.0.0-1", "5.0.0-2",
    "6.0.0", "6.0.0-1", "6.1.0",
  }
  local i = 0
  return function ()
    i = i + 1
    return versions[i]
  end
end

-- Sanity check for match code
assert(match("0.0.1", iterator()) == "0.0.1")
assert(match("0.0.3", iterator()) == "0.0.3-2")
assert(match("0.1.0", iterator()) == "0.1.1")
assert(match("0.1.0-1", iterator()) == "0.1.1")
assert(match("0.2.0", iterator()) == "0.2.1")
assert(match("0.3.0", iterator()) == "0.3.1")
assert(match("0.4.0", iterator()) == "0.4.0-2")
assert(not match("0.5.0", iterator()))
assert(match("1.0.0", iterator()) == "1.1.3")
assert(match("1.0.0-1", iterator()) == "1.1.3")
assert(not match("1.1.4", iterator()))
assert(not match("1.2.0", iterator()))
assert(match("2.0.0", iterator()) == "2.1.2")
assert(not match("2.1.3", iterator()))
assert(not match("2.2.0", iterator()))
assert(match("3.0.0", iterator()) == "3.1.4")
assert(match("4.0.0", iterator()) == "4.0.1")
assert(match("5.0.0", iterator()) == "5.0.0-2")
assert(match("6.0.0", iterator()) == "6.1.0")
assert(not match("3.1.5", iterator()))
assert(match(nil, iterator()) == "6.1.0")

return exports
