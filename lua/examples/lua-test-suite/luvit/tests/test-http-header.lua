local headerMeta = require('http-header').headerMeta
local newHeaders = require('http-header').newHeaders
local toHeaders = require('http-header').toHeaders
local combineHeaders = require('http-header').combineHeaders
local getHeaders = require('http-header').getHeaders
local deepEqual = require('deep-equal')

require('tap')(function(test)

  test("Set via string", function()
    local headers = setmetatable({}, headerMeta)
    headers.Game = "Monkey Ball"
    headers.Color = "Many"
    p(headers)
    assert(#headers == 2)
    assert(headers.game == "Monkey Ball")
    assert(headers.color == "Many")
  end)

  test("Set via append", function()
    local headers = setmetatable({}, headerMeta)
    headers[#headers + 1] = {"Game", "Monkey Ball"}
    headers[#headers + 1] = {"Color", "Many"}
    p(headers)
    assert(#headers == 2)
    assert(headers.game == "Monkey Ball")
    assert(headers.color == "Many")
  end)

  test("Replace header", function()
    local headers = setmetatable({}, headerMeta)
    headers.Game = "Monkey Ball"
    headers.Game = "Ultimate"
    p(headers)
    assert(#headers == 1)
    assert(headers.game == "Ultimate")
  end)

  test("Duplicate Keys", function()
    local headers = setmetatable({}, headerMeta)
    headers[#headers + 1] = {"Skill", "Network"}
    headers[#headers + 1] = {"Skill", "Compute"}
    p(headers)
    assert(#headers == 2)
    assert(headers[1][2] == "Network")
    assert(headers[2][2] == "Compute")
    assert(headers.skill == "Network" or headers.skill == "Compute")
  end)

  test("Remove Keys", function()
    local headers = setmetatable({
      {"Color", "Blue"},
      {"Color", "Red"},
      {"Color", "Green"},
    }, headerMeta)
    p(headers)
    assert(#headers == 3)
    headers.color = nil
    p(headers)
    assert(#headers == 0)
  end)

  test("Replace Keys", function()
    local headers = setmetatable({
      {"Color", "Blue"},
      {"Color", "Red"},
      {"Color", "Green"},
    }, headerMeta)
    p(headers)
    assert(#headers == 3)
    headers.Color = "Orange"
    p(headers)
    assert(#headers == 1)
    assert(headers.Color == "Orange")
  end)

  test("Replace Keys with Keys", function()
    local headers = setmetatable({
      {"Color", "Blue"},
      {"Color", "Red"},
      {"Color", "Green"},
    }, headerMeta)
    p(headers)
    assert(#headers == 3)
    headers.Color = { "Orange", "Purple" }
    p(headers)
    assert(#headers == 2)
    assert(headers[1][2] == "Orange")
    assert(headers[2][2] == "Purple")
  end)

  test("Large test", function()
    local headers = setmetatable({
      {"Game", "Monkey Ball"},
      {"Game", "Ultimate"},
      {"Skill", "Network"},
      {"Skill", "Compute"},
      {"Color", "Blue"},
      {"Color", "Red"},
      {"Color", "Green"},
    }, headerMeta)
    headers.Why = "Because"
    p(headers)
    assert(#headers == 8)
    if headers.cOLOR then
      headers.Color = "Many"
    end
    if headers.gAME then
      headers.Game = "Yes"
    end
    p(headers)
    assert(#headers == 5)
    assert(headers.game == "Yes")
    assert(headers.color == "Many")
    assert(headers.why == "Because")
  end)

  test("Converting headers", function()
    local headers = {
      ["Game"] = {"Monkey Ball", "Ultimate"},
      ["Skill"] = {"Network", "Compute"},
      {"Color", "Blue"},
      {"Color", "Red"},
      {"Color", "Green"},
    }
    headers.Why = "Because"
    p(headers)
    headers = toHeaders(headers)
    p(headers)
    assert(#headers == 8)
    if headers.cOLOR then
      headers.Color = "Many"
    end
    if headers.gAME then
      headers.Game = "Yes"
    end
    p(headers)
    assert(#headers == 5)
    assert(headers.game == "Yes")
    assert(headers.color == "Many")
    assert(headers.why == "Because")
  end)

  test("Converting headers multiple times", function()
    local headers = {
      ["Game"] = {"Monkey Ball", "Ultimate"},
      ["Skill"] = {"Network", "Compute"},
      {"Color", "Blue"},
      {"Color", "Red"},
      {"Color", "Green"},
    }
    headers.Why = "Because"
    local first = toHeaders(headers)
    p(first)
    local second = toHeaders(first)
    p(second)
    local third = toHeaders(second)
    p(third)
    assert(#first == 8)
    assert(#second == 8)
    assert(#third == 8)
    assert(deepEqual(first, third))
  end)

  test("Combining headers", function()
    local headers1 = {
      ["Game"] = {"Monkey Ball", "Ultimate"},
      ["Skill"] = {"Network", "Compute"},
    }
    headers1.Why = "Because"
    local headers2 = {
      {"Color", "Blue"},
      {"Color", "Red"},
      {"Color", "Green"},
    }
    local headers3 = {}
    headers3.Why = "Duplicate key will overwrite"
    p(headers1)
    p(headers2)
    p(headers3)
    local headers = combineHeaders(headers1, headers2, headers3)
    p(headers)
    assert(#headers == 8)
    assert(headers.Why == "Duplicate key will overwrite")
  end)

  test("Extracting headers", function()
    local res = {
      {"Color", "Blue"},
      {"Game", "Monkey Ball"},
      {"Skill", "Network"},
      ["ignored"] = true,
      ["fn"] = function() end
    }
    p(res)
    local headers = getHeaders(res)
    p(headers)
    assert(#headers == 3)
    assert(headers.game == "Monkey Ball")
    assert(headers.color == "Blue")
    assert(headers.skill == "Network")
  end)

  test("Extracting headers from headerMeta table", function()
    local headers = toHeaders {
      ["Game"] = {"Monkey Ball", "Ultimate"},
      ["Skill"] = "Network",
    }
    local gottenHeaders = getHeaders(headers)
    p(headers)
    p(gottenHeaders)
    assert(#headers == 3)
    assert(#gottenHeaders == 3)
    assert(deepEqual(headers, gottenHeaders))
  end)

end)
