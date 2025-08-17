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

local function BottomUpTree(item, depth)
  if depth > 0 then
    local i = item + item
    depth = depth - 1
    local left, right = BottomUpTree(i-1, depth), BottomUpTree(i, depth)
    return { item, left, right }
  else
    return { item }
  end
end

local function ItemCheck(tree)
  if tree[2] then
    return tree[1] + ItemCheck(tree[2]) - ItemCheck(tree[3])
  else
    return tree[1]
  end
end

local N = tonumber(arg and arg[1]) or 10

local mindepth = 4
local maxdepth = mindepth + 2
if maxdepth < N then maxdepth = N end

do
  local stretchdepth = maxdepth + 1
  local stretchtree = BottomUpTree(0, stretchdepth)
  print(string.format("stretch tree of depth %d\t check: %d\n",
    stretchdepth, ItemCheck(stretchtree)))
end

local longlivedtree = BottomUpTree(0, maxdepth)

for depth=mindepth,maxdepth,2 do
  local iterations = 2 ^ (maxdepth - depth + mindepth)
  local check = 0
  for i=1,iterations do
    check = check + ItemCheck(BottomUpTree(1, depth)) +
            ItemCheck(BottomUpTree(-1, depth))
  end
  print(string.format("%d\t trees of depth %d\t check: %d\n",
    iterations*2, depth, check))
end

print(string.format("long lived tree of depth %d\t check: %d\n",
  maxdepth, ItemCheck(longlivedtree)))

end

bench.runCode(test, "binary-trees")
