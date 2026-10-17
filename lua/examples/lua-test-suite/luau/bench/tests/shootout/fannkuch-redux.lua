--[[
MIT License

Copyright (c) 2017 Gabriel de Quadros Ligneul

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
]]
-- The Computer Language Benchmarks Game
-- http://benchmarksgame.alioth.debian.org/
-- contributed by Mike Pall

local bench = script and require(script.Parent.bench_support) or require("bench_support")

function test()

local function fannkuch(n)
  local p, q, s, sign, maxflips, sum = {}, {}, {}, 1, 0, 0
  for i=1,n do p[i] = i; q[i] = i; s[i] = i end
  repeat
    -- Copy and flip.
    local q1 = p[1]				-- Cache 1st element.
    if q1 ~= 1 then
      for i=2,n do q[i] = p[i] end		-- Work on a copy.
      local flips = 1
      repeat
	local qq = q[q1]
	if qq == 1 then				-- ... until 1st element is 1.
	  sum = sum + sign*flips
	  if flips > maxflips then maxflips = flips end -- New maximum?
	  break
	end
	q[q1] = q1
	if q1 >= 4 then
	  local i, j = 2, q1 - 1
	  repeat q[i], q[j] = q[j], q[i]; i = i + 1; j = j - 1; until i >= j
	end
	q1 = qq; flips = flips + 1
      until false
    end
    if sign == 1 then
      p[2], p[1] = p[1], p[2]; sign = -1	-- Rotate 1<-2.
    else
      p[2], p[3] = p[3], p[2]; sign = 1		-- Rotate 1<-2 and 1<-2<-3.
      for i=3,n do
	local sx = s[i]
	if sx ~= 1 then s[i] = sx-1; break end
	if i == n then return sum, maxflips end	-- Out of permutations.
	s[i] = i
	-- Rotate 1<-...<-i+1.
	local t = p[1]; for j=1,i do p[j] = p[j+1] end; p[i+1] = t
      end
    end
  until false
end

local n = tonumber(arg and arg[1]) or 8
local sum, flips = fannkuch(n)
print(sum, "\nPfannkuchen(", n, ") = ", flips, "\n")

end

bench.runCode(test, "fannkuchen-redux")
