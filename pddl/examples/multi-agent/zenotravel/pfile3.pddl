(define (problem ZTRAVEL-2-4) (:domain zeno-travel)
(:objects
	plane2 - aircraft
	fl1 - flevel
	fl0 - flevel
	fl3 - flevel
	fl2 - flevel
	fl5 - flevel
	fl4 - flevel
	fl6 - flevel
	person2 - person
	person3 - person
	person1 - person
	person4 - person
	city2 - city
	city0 - city
	city1 - city

	(:private plane1
		plane1 - aircraft
	)
)
(:init
	(at plane1 city0)
	(fuel-level plane1 fl4)
	(at plane2 city2)
	(fuel-level plane2 fl5)
	(at person1 city0)
	(at person2 city0)
	(at person3 city1)
	(at person4 city1)
	(next fl0 fl1)
	(next fl1 fl2)
	(next fl2 fl3)
	(next fl3 fl4)
	(next fl4 fl5)
	(next fl5 fl6)
)
(:goal
	(and
		(at plane2 city2)
		(at person1 city1)
		(at person2 city0)
		(at person3 city0)
		(at person4 city1)
	)
)
)