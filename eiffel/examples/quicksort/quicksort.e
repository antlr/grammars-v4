class
	QUICKSORT [G -> COMPARABLE]

create
	make

feature {NONE} --Implementation

	is_sorted (list: ARRAY [G]): BOOLEAN
		require
			not_void: list /= Void
		local
			i: INTEGER
		do
			Result := True
			from
				i := list.lower + 1
			invariant
				i >= list.lower + 1 and i <= list.upper + 1
			until
				i > list.upper
			loop
				Result := Result and list [i - 1] <= list [i]
				i := i + 1
			variant
				list.upper + 1 - i
			end
		end

	concatenate_array (a: ARRAY [G] b: ARRAY [G]): ARRAY [G]
		require
			not_void: a /= Void and b /= Void
		do
			create Result.make_from_array (a)
			across
				b as t
			loop
				Result.force (t.item, Result.upper + 1)
			end
		ensure
			same_size: a.count + b.count = Result.count
		end

	quicksort_array (list: ARRAY [G]): ARRAY [G]
		require
			not_void: list /= Void
		local
			less_a: ARRAY [G]
			equal_a: ARRAY [G]
			more_a: ARRAY [G]
			pivot: G
		do
			create less_a.make_empty
			create more_a.make_empty
			create equal_a.make_empty
			create Result.make_empty
			if list.count <= 1 then
				Result := list
			else
				pivot := list [list.lower]
				across
					list as li
				invariant
					less_a.count + equal_a.count + more_a.count <= list.count
				loop
					if li.item < pivot then
						less_a.force (li.item, less_a.upper + 1)
					elseif li.item = pivot then
						equal_a.force (li.item, equal_a.upper + 1)
					elseif li.item > pivot then
						more_a.force (li.item, more_a.upper + 1)
					end
				end
				Result := concatenate_array (Result, quicksort_array (less_a))
				Result := concatenate_array (Result, equal_a)
				Result := concatenate_array (Result, quicksort_array (more_a))
			end
		ensure
			same_size: list.count = Result.count
			sorted: is_sorted (Result)
		end

feature -- Initialization

	make
		do
		end

	quicksort (a: ARRAY [G]): ARRAY [G]
		do
			Result := quicksort_array (a)
		end

end