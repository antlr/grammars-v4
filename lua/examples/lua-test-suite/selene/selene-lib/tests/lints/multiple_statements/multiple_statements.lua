table.sort(stuff, function(a, b)
    local sum = a + b
    return math.abs(sum) > 2
end)

table.sort(stuff, function(a, b) local sum = a + b return math.abs(sum) > 2 end)
