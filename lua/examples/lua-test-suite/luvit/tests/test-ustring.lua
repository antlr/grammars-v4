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

--[[!!!
    NOTICE:
    This file contains some Emoji character.
    Your editor may display them incorrectly, but please DO NOT delete them :)
]]

local ustring = require('ustring')

local function compareTable(t1,t2)
    if #t1 ~= #t2 then return false end
    for i = 1,#t1 do
        if rawget(t1,i) ~= rawget(t2,i) then return false end
    end
    return true
end

require('tap')(function(test)
  test('ustring', function(expect)

    -- construct & tostring
    local r = "abc123\11\27\0中文-日本語-한국❤😘😄💎💛"
    local u = ustring.new(r)
    local e = {
    'a','b','c','1','2','3','\11','\27','\0',
    '中','文','-','日','本','語','-','한','국', -- Chinese - Japanese - Korean
    '❤','😘','😄','💎','💛'-- Emoji
    }
    assert(compareTable(u,e))
    assert(tostring(u) == r) -- __tostring
    local u2 = u:copy()
    assert(compareTable(u2,u))
    assert(compareTable(u2,e))
    assert(u2 == u) -- __eq

    -- uindex
    assert(u:index2uindex(13) == 11)
    assert(u:index2uindex(43) == 21)
    assert(u:index2uindex(13,10,10) == 11)
    assert(u:index2uindex(43,39,20) == 21)

    -- other
    local u = ustring.new "xx中x中😄" .. ustring.new "文💛"
    assert(tostring(u) == "xx中x中😄文💛")

    assert(tostring(u:sub(3,-3)) == "中x中😄")
    assert(tostring(u:sub(-2,7)) == '文')

    local first,last = u:find("中x中")
    assert(first == 3)
    assert(last == 5)
    local first,last = u:find("中",4)
    assert(first == 5)
    assert(last == 5)

    assert(tostring(ustring.format(ustring.new "格式%s测试：,%i","文本",4685)) == "格式文本测试：,4685")

    assert(tostring(ustring.new("abcde参杂fghijk"):upper()) == "ABCDE参杂FGHIJK")
    assert(tostring(ustring.new("ABCDEFGHIJK"):lower()) == "abcdefghijk")

    assert(tostring(ustring.rep(ustring.new"💙💛💙💜",4)) == "💙💛💙💜💙💛💙💜💙💛💙💜💙💛💙💜")

    assert(tostring(ustring.new("翻转测试reverse test"):reverse()) == "tset esrever试测转翻")

  end)
end)
