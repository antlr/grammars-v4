--[[
   The Great Computer Language Shootout
   http://shootout.alioth.debian.org/
   contributed by Isaac Gouy
]]
local bench = script and require(script.Parent.bench_support) or require("bench_support")

function test()

local function ack(m,n)
    if (m==0) then return n+1; end
    if (n==0) then return ack(m-1,1); end
    return ack(m-1, ack(m,n-1) );
end
 
local function fib(n)
    if (n < 2) then return 1; end
    return fib(n-2) + fib(n-1);
end

local function tak(x,y,z)
    if (y >= x) then return z; end
    return tak(tak(x-1,y,z), tak(y-1,z,x), tak(z-1,x,y));
end

local result = 0;

for i = 3,5 do
    result = result + ack(3,i);
    result = result + fib(17+i);
    result = result + tak(3*i+3,2*i+2,i+1);
end

local expected = 57775;

if (result ~= expected) then
    assert(false, "ERROR: bad result: expected " .. expected .. " but got " .. result);
end

end

bench.runCode(test, "controlflow-recursive")
