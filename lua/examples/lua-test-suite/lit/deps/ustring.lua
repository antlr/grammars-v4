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
--[[lit-meta
  name = "luvit/ustring"
  version = "2.0.2"
  homepage = "https://github.com/luvit/luvit/blob/master/deps/ustring.lua"
  description = "A light-weight UTF-8 module in pure lua(jit)."
  tags = {"ustring", "utf8", "utf-8", "unicode"}
  license = "Apache 2"
]]

local ustring = {}
local tostring = tostring
local _meta = {}

local strsub = string.sub
local strbyte = string.byte
local rshift = bit.rshift

local function chlen(byte)
    if rshift(byte,7) == 0x00 then
        return 1
    elseif rshift(byte,5) == 0x06 then
        return 2
    elseif rshift(byte,4) == 0x0E then
        return 3
    elseif rshift(byte,3) == 0x1E then
        return 4
    else
        -- RFC 3629 (2003.11) says UTF-8 don't have characters larger than 4 bytes.
        -- They will not be processed although they may be appeared in some old systems.
        return 0
    end
end
ustring.chlen = chlen

function ustring.new(str,allowInvaild)
    str = str and tostring(str) or ""
    local ustr = {}
    local index = 1;
    local append = 0;
    for i = 1,#str do
        repeat
        local char = strsub(str,i,i)
        local byte = strbyte(char)
        if append ~= 0 then
            if not allowInvaild then
                if rshift(byte,6) ~= 0x02 then
                    error("Invaild UTF-8 sequence at "..i)
                end
            end
            ustr[index] = ustr[index] .. char
            append = append - 1
            if append == 0 then
                index = index + 1
            end
            break
        end

        local charLen = chlen(byte)
        if charLen == 0 and not allowInvaild then error("Invaild UTF-8 sequence at "..tostring(i)..",byte:"..tostring(byte)) end
        ustr[index] = char
        if charLen == 1 then index = index + 1 end
        append = append + charLen - 1

        until true
    end
    setmetatable(ustr,_meta)
    return ustr
end

function ustring.copy(ustr)
    local u = ustring.new()
    for i = 1,#ustr do
        u[i] = ustr[i]
    end
    return u
end

function ustring.index2uindex(ustr,rawindex,initrawindex,initindex)
    -- convert a raw index into the index of a UTF-8
    -- return `nil` if uindex is invaild
    -- the last 2 arguments are optional and used for better performance (only if rawindex isn't negative)
    if rawindex < 0 then
        local index = 1
        repeat
            local uchar = ustr[index]
            if uchar == nil then return nil end
            local len = #uchar
            index = index + 1
            rawindex = rawindex + len
        until rawindex >= 0
        return -(index - 1)
    else
        rawindex = rawindex - (initrawindex or 1) + 1
        local index = (initindex or 1)
        repeat
            local uchar = ustr[index]
            if uchar == nil then return nil end
            local len = #uchar
            index = index + 1
            rawindex = rawindex - len
        until rawindex <= 0
        return index - 1
    end
end

function ustring.uindex2index(ustr,uindex,initrawindex,inituindex)
    -- convert the index of a UIF-8 char into a raw index
    -- return `nil` if rawindex is invaild
    -- the last 2 arguments are optional and used for better performance (only if uindex isn't negative)
    uindex = uindex or 1
    local ulen = #ustr
    if uindex < 0 then
        local index = 0
        for i = ulen,ulen + uindex + 1,-1 do
            index = index + #ustr[i]
        end
        return -index
    else
        local index = (inituindex or 1)
        inituindex = inituindex or 1
        for i = inituindex,uindex - 1 do
            index = index + #ustr[i]
        end
        return index
    end
end

local gsub = string.gsub
local find = string.find
local format = string.format
local gmatch = string.gmatch
local match = string.match
local lower = string.lower
local upper = string.upper
ustring.len = rawlen

function ustring.gsub(ustr,pattern,repl,n)
    return ustring.new(gsub(tostring(ustr),tostring(pattern),tostring(repl),n))
end

function ustring.sub(ustr,i,j)
    local u = ustring.new()
    j = j or -1
    local len = #ustr
    if i < 0 then i = len + i + 1 end
    if j < 0 then j = len + j + 1 end
    for ii = i,math.min(j,len) do
        u[#u + 1] = ustr[ii]
    end
    return u
end

function ustring.find(ustr,pattern,init,plain)
    local first,last = find(tostring(ustr),tostring(pattern),ustring.uindex2index(ustr,init),plain)
    if first == nil then return nil end
    local ufirst = ustring.index2uindex(ustr,first)
    local ulast = ustring.index2uindex(ustr,last,first,ufirst)
    return ufirst,ulast
end

function ustring.format(formatstring,...)
    return ustring.new(format(tostring(formatstring),...))
end

function ustring.gmatch(ustr,pattern)
    return gmatch(tostring(ustr),pattern)
end

function ustring.match(ustr,pattern,init)
    return match(tostring(ustr),tostring(pattern),ustring.uindex2index(ustr,init))
end

function ustring.lower(ustr)
    local u = ustring.copy(ustr)
    for i = 1,#u do
        u[i] = lower(u[i])
    end
    return u
end

function ustring.upper(ustr)
    local u = ustring.copy(ustr)
    for i = 1,#u do
        u[i] = upper(u[i])
    end
    return u
end

function ustring.rep(ustr,n)
    local u = ustring.new()
    for i = 1,n do
        for ii = 1,#ustr do
           u[#u + 1] = ustr[ii]
        end
    end
    return u
end

function ustring.reverse(ustr)
    local u = ustring.copy(ustr)
    local len = #ustr;
    for i = 1,len do
        u[i] = ustr[len - i + 1]
    end
    return u
end

_meta.__index = ustring

function _meta.__eq(ustr1,ustr2)
    local len1 = #ustr1
    local len2 = #ustr2
    if len1 ~= len2 then return false end

    for i = 1,len1 do
        if ustr1[i] ~= ustr2[i] then return false end
    end
    return true
end

function _meta.__tostring(self)
    return tostring(table.concat(self))
end

function _meta.__concat(ustr1,ustr2)
    local u = ustring.copy(ustr1)
    for i = 1,#ustr2 do
        u[#u + 1] = ustr2[i]
    end
    return u
end

_meta.__len = ustring.len

return ustring
