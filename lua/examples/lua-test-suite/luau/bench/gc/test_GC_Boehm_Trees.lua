--!non-strict
local bench = script and require(script.Parent.bench_support) or require("bench_support")

local stretchTreeDepth = 18 -- about 16Mb
local longLivedTreeDepth = 16 -- about 4Mb
local arraySize = 500000 --about 4Mb
local minTreeDepth = 4
local maxTreeDepth = 16

-- Nodes used by a tree of a given size
function treeSize(i)
    return bit32.lshift(1, i + 1) - 1
end

function getNumIters(i)
    return 2 * treeSize(stretchTreeDepth) / treeSize(i)
end

-- Build tree top down, assigning to older objects. 
function populate(depth, thisNode)
    if depth <= 0 then
        return
    end

    depth = depth - 1
    thisNode.left  = {}
    thisNode.right = {}
    populate(depth, thisNode.left)
    populate(depth, thisNode.right)
end

-- Build tree bottom-up
function makeTree(depth)
    if depth <= 0 then
        return {}
    end

    return { left = makeTree(depth - 1), right = makeTree(depth - 1) }
end

function timeConstruction(depth)
    local numIters = getNumIters(depth)
    local tempTree = {}

    for i = 1, numIters do
        tempTree = {}
        populate(depth, tempTree)
        tempTree = nil
    end

    for i = 1, numIters do
        tempTree = makeTree(depth)
        tempTree = nil
    end
end

function test()
    -- Stretch the memory space quickly
    local _tempTree = makeTree(stretchTreeDepth)
    _tempTree = nil

    -- Create a long lived object
    local longLivedTree = {}
    populate(longLivedTreeDepth, longLivedTree)

    -- Create long-lived array, filling half of it
    local array = {}
    for i = 1, arraySize/2 do
        array[i] = 1.0 / i
    end

    for d = minTreeDepth,maxTreeDepth,2 do
        timeConstruction(d)
    end
end

bench.runs = 6
bench.extraRuns = 2

bench.runCode(test, "GC: Boehm tree")
