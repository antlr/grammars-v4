(define (problem ZTRAVEL-2-6) (:domain zeno-travel)
(:objects
	person2 - person
	person3 - person
	person1 - person
	person6 - person
	person4 - person
	person5 - person
	city2 - city
	city3 - city
	city0 - city
	city1 - city
	fl1 - flevel
	fl0 - flevel
	fl3 - flevel
	fl2 - flevel
	fl5 - flevel
	fl4 - flevel
	fl6 - flevel
	plane2 - aircraft

	(:private plane1
		plane1 - aircraft
	)
)
(:init
	(at plane1 city2)
	(fuel-level plane1 fl1)
	(at plane2 city1)
	(fuel-level plane2 fl1)
	(at person1 city3)
	(at person2 city3)
	(at person3 city3)
	(at person4 city1)
	(at person5 city3)
	(at person6 city0)
	(next fl0 fl1)
	(next fl1 fl2)
	(next fl2 fl3)
	(next fl3 fl4)
	(next fl4 fl5)
	(next fl5 fl6)
)
(:goal
	(and
		(at plane2 city1)
		(at person1 city2)
		(at person3 city3)
		(at person4 city3)
		(at person5 city2)
		(at person6 city2)
	)
)
)