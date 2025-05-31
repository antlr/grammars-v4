local bench = script and require(script.Parent.bench_support) or require("bench_support")

function test()
    local count = 1

    local function fill_tree(tree, levels)
        if not tree.left then
            tree.left = { id = count }
            count = count + 1
        end
    
        if not tree.right then
            tree.right = { id = count }
            count = count + 1
        end
    
        if levels ~= 0 then
            fill_tree(tree.left, levels - 1)
            fill_tree(tree.right, levels - 1)
        end
    end
    
    local function prune_tree(tree, level)
        if tree.left then
            if math.random() > 0.9 - level * 0.05 then
                tree.left = nil
            else
                prune_tree(tree.left, level + 1)
            end
        end

        if tree.right then
            if math.random() > 0.9 - level * 0.05 then
                tree.right = nil
            else
                prune_tree(tree.right, level + 1)
            end
        end
    end

    -- create a static tree
	local tree = { id = 0 }
	fill_tree(tree, 16)

	for i = 1,100 do
		local small_tree = { id = 0 }

		fill_tree(small_tree, 8)

		prune_tree(small_tree, 0)
	end
end

bench.runCode(test, "GC: tree pruning (eager fill, gen)")
