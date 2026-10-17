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
local bench = script and require(script.Parent.bench_support) or require("bench_support")

function test()


local random, floor = math.random, math.floor
floor = math.ifloor or floor

function heapsort(n, ra)
    local j, i, rra
    local l = floor(n/2) + 1
    -- local l = (n//2) + 1
    local ir = n;
    while 1 do
        if l > 1 then
            l = l - 1
            rra = ra[l]
        else
            rra = ra[ir]
            ra[ir] = ra[1]
            ir = ir - 1
            if (ir == 1) then
                ra[1] = rra
                return
            end
        end
        i = l
        j = l * 2
        while j <= ir do
            if (j < ir) and (ra[j] < ra[j+1]) then
                j = j + 1
            end
            if rra < ra[j] then
                ra[i] = ra[j]
                i = j
                j = j + i
            else
                j = ir + 1
            end
        end
        ra[i] = rra
    end
end

local Num = tonumber((arg and arg[1])) or 4
for i=1,Num do
  local N = tonumber((arg and arg[2])) or 10000
  local a = {}
  for i=1,N do a[i] = random() end
  heapsort(N, a)
  for i=1,N-1 do assert(a[i] <= a[i+1]) end
end

end

bench.runCode(test, "heapsort")
