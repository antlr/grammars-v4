class
	APPLICATION

create
	make

feature {NONE} -- Initialization

	make
			-- Run application.
		local
			test: ARRAY [INTEGER]
			sorted: ARRAY [INTEGER]
			sorter: QUICKSORT [INTEGER]
		do
			create sorter.make
			test := <<1, 3, 2, 4, 5, 5, 7, -1>>
			sorted := sorter.quicksort (test)
			across
				sorted as s
			loop
				print (s.item)
				print (" ")
			end
			print ("%N")
		end

end