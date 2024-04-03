(define (problem ZTRAVEL-5-15) (:domain zeno-travel)
(:objects
	person14 - person
	person15 - person
	person10 - person
	person11 - person
	person12 - person
	person13 - person
	person2 - person
	person3 - person
	city8 - city
	person1 - person
	person6 - person
	person7 - person
	person4 - person
	person5 - person
	city2 - city
	city3 - city
	person8 - person
	city1 - city
	city6 - city
	city7 - city
	city4 - city
	city5 - city
	person9 - person
	fl1 - flevel
	fl0 - flevel
	fl3 - flevel
	fl2 - flevel
	fl5 - flevel
	fl4 - flevel
	fl6 - flevel
	city0 - city
	plane3 - aircraft
	plane2 - aircraft
	city10 - city
	city11 - city
	city12 - city
	city13 - city
	city9 - city

	(:private plane1
		plane1 - aircraft
	)

	(:private plane4
		plane4 - aircraft
	)

	(:private plane5
		plane5 - aircraft
	)
)
(:init
	(at plane1 city6)
	(fuel-level plane1 fl2)
	(at plane2 city0)
	(fuel-level plane2 fl3)
	(at plane3 city10)
	(fuel-level plane3 fl5)
	(at plane4 city4)
	(fuel-level plane4 fl4)
	(at plane5 city1)
	(fuel-level plane5 fl6)
	(at person1 city8)
	(at person2 city12)
	(at person3 city0)
	(at person4 city4)
	(at person5 city13)
	(at person6 city7)
	(at person7 city1)
	(at person8 city2)
	(at person9 city1)
	(at person10 city2)
	(at person11 city10)
	(at person12 city7)
	(at person13 city6)
	(at person14 city1)
	(at person15 city13)
	(next fl0 fl1)
	(next fl1 fl2)
	(next fl2 fl3)
	(next fl3 fl4)
	(next fl4 fl5)
	(next fl5 fl6)
)
(:goal
	(and
		(at plane2 city12)
		(at plane3 city6)
		(at person1 city3)
		(at person2 city4)
		(at person3 city11)
		(at person4 city13)
		(at person5 city11)
		(at person6 city7)
		(at person7 city1)
		(at person8 city11)
		(at person9 city2)
		(at person10 city6)
		(at person11 city0)
		(at person12 city12)
		(at person13 city13)
		(at person14 city4)
		(at person15 city4)
	)
)
)