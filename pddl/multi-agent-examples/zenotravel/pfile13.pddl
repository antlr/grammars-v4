(define (problem ZTRAVEL-3-10) (:domain zeno-travel)
(:objects
	person10 - person
	person2 - person
	person3 - person
	person1 - person
	person6 - person
	person7 - person
	person4 - person
	person5 - person
	city2 - city
	city3 - city
	city0 - city
	person9 - person
	person8 - person
	city4 - city
	city5 - city
	city1 - city
	fl1 - flevel
	fl0 - flevel
	fl3 - flevel
	fl2 - flevel
	fl5 - flevel
	fl4 - flevel
	fl6 - flevel
	plane1 - aircraft

	(:private plane2
		plane2 - aircraft
	)

	(:private plane3
		plane3 - aircraft
	)
)
(:init
	(at plane1 city4)
	(fuel-level plane1 fl2)
	(at plane2 city3)
	(fuel-level plane2 fl6)
	(at plane3 city3)
	(fuel-level plane3 fl6)
	(at person1 city1)
	(at person2 city2)
	(at person3 city1)
	(at person4 city4)
	(at person5 city5)
	(at person6 city1)
	(at person7 city0)
	(at person8 city2)
	(at person9 city1)
	(at person10 city5)
	(next fl0 fl1)
	(next fl1 fl2)
	(next fl2 fl3)
	(next fl3 fl4)
	(next fl4 fl5)
	(next fl5 fl6)
)
(:goal
	(and
		(at plane1 city4)
		(at person1 city4)
		(at person2 city5)
		(at person3 city4)
		(at person4 city0)
		(at person5 city2)
		(at person6 city3)
		(at person8 city0)
		(at person9 city3)
		(at person10 city4)
	)
)
)