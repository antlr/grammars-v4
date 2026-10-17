--[[
   The Great Computer Language Shootout
   http://shootout.alioth.debian.org/
   contributed by Isaac Gouy
]]
local bench = script and require(script.Parent.bench_support) or require("bench_support")

function test()

local function partial(n)
    local a1, a2, a3, a4, a5, a6, a7, a8, a9 = 0, 0, 0, 0, 0, 0, 0, 0, 0;
    local twothirds = 2.0/3.0;
    local alt = -1.0;
    local k2, k3, sk, ck = 0, 0, 0, 0;
    
    for k = 1,n do
        k2 = k*k;
        k3 = k2*k;
        sk = math.sin(k);
        ck = math.cos(k);
        alt = -alt;
        
        a1 = a1 + math.pow(twothirds,k-1);
        a2 = a2 + math.pow(k,-0.5);
        a3 = a3 + 1.0/(k*(k+1.0));
        a4 = a4 + 1.0/(k3 * sk*sk);
        a5 = a5 + 1.0/(k3 * ck*ck);
        a6 = a6 + 1.0/k;
        a7 = a7 + 1.0/k2;
        a8 = a8 + alt/k;
        a9 = a9 + alt/(2*k -1);
    end
    
    return a6 + a7 + a8 + a9;
end

local total = 0;
local i = 1024

while i <= 16384 do
    total = total + partial(i);
    i = i * 2
end

local expected = 60.08994194659945;

if (total ~= expected) then
    assert(false, "ERROR: bad result: expected " .. expected .. " but got " .. total);
end

end

bench.runCode(test, "math-partial-sums")
