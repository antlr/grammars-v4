-- This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
local bench = {}

bench.runs = 20
bench.extraRuns = 4

function bench.runCode(f, description)
    -- Under Callgrind, run the test only once and measure just the execution cost
    if callgrind and callgrind("running") then
        if collectgarbage then collectgarbage() end

        callgrind("zero")
        f() -- unfortunately we can't easily separate setup cost from runtime cost in f unless it calls callgrind()
        callgrind("dump", description)
        return
    end

    local timeTable = {}

    for i = 1,bench.runs + bench.extraRuns do
        -- try to run GC if it's available
        if collectgarbage then
            pcall(function()
                collectgarbage()
            end)
        end

        local ts0 = os.clock()

        local result = f()

        local ts1 = os.clock()

        -- If test case doesn't return a duration (if only a part of code is measured) we will measure full execution time here
        if not result then
            result = ts1 - ts0
        end

        table.insert(timeTable, result)
    end

    table.sort(timeTable)

    for i = 1,bench.extraRuns do
        table.remove(timeTable, #timeTable)
    end

    -- Output test name followed by each result
    local report = "|><|"..description

    for _,v in ipairs(timeTable) do
        report = report .. "|><|" .. (v * 1000)
    end

    report = report .. "||_||"

    print(report)
end

return bench
