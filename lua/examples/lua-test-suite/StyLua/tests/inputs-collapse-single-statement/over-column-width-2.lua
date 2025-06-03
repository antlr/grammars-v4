T['stat_summary()']['works'] = function()
	eq(stat_summary(10, 4, 3, 2, 1), { minimum = 1, mean = 4, median = 3, maximum = 10, n = 5, sd = math.sqrt(50 / 4) })
end
