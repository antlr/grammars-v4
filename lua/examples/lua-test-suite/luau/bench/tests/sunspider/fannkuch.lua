--[[
   The Great Computer Language Shootout
   http://shootout.alioth.debian.org/
   contributed by Isaac Gouy
]]
local bench = script and require(script.Parent.bench_support) or require("bench_support")

function test()

local function fannkuch(n)
   local check = 0;
   local perm = {};
   local perm1 = {};
   local count = {};
   local maxPerm = {};
   local maxFlipsCount = 0;
   local m = n - 1;

   for i = 1,n do perm1[i] = i - 1; end
   local r = n;

   while (true) do
      -- write-out the first 30 permutations
      if (check < 30) then
         local s = "";
         for i = 1,n do s = s .. tostring(perm1[i]+1); end
         check = check + 1;
      end

      while (r ~= 1) do count[r] = r; r = r - 1; end

      if (not (perm1[1] == 0 or perm1[m + 1] == m)) then
         for i = 1,n do perm[i] = perm1[i]; end

         local flipsCount = 0;
         local k;

         k = perm[1]

         while (not (k == 0)) do
            local k2 = math.floor((k + 1) / 2);
            for i = 0,k2-1 do
                local temp = perm[i + 1];
                perm[i + 1] = perm[k - i + 1];
                perm[k - i + 1] = temp;
             end

            flipsCount = flipsCount + 1;

            k = perm[1]
        end

         if (flipsCount > maxFlipsCount) then
            maxFlipsCount = flipsCount;
            for i = 1,n do maxPerm[i] = perm1[i]; end
         end
        end

      while (true) do
         if (r == n) then return maxFlipsCount; end

         local perm0 = perm1[1];
         local i = 0;
         while (i < r) do
            local j = i + 1;
            perm1[i + 1] = perm1[j + 1];
            i = j;
         end
         perm1[r + 1] = perm0;

         count[r + 1] = count[r + 1] - 1;
         if (count[r + 1] > 0) then break; end
         r = r + 1;
        end
    end

    return 0
end

local n = 8;
local ret = fannkuch(n);

local expected = 22;
if (ret ~= expected) then
    assert(false, "ERROR: bad result: expected " .. expected .. " but got " .. ret);
end

end

bench.runCode(test, "fannkuch")
