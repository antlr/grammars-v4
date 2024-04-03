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
	city9 - city
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
	city10 - city
	city11 - city
	person1 - person

	(:private plane1
		plane1 - aircraft
	)

	(:private plane2
		plane2 - aircraft
	)

	(:private plane3
		plane3 - aircraft
	)

	(:private plane4
		plane4 - aircraft
	)

	(:private plane5
		plane5 - aircraft
	)
)
(:init
	(at plane1 city0)
	(fuel-level plane1 fl0)
	(at plane2 city3)
	(fuel-level plane2 fl4)
	(at plane3 city2)
	(fuel-level plane3 fl1)
	(at plane4 city9)
	(fuel-level plane4 fl1)
	(at plane5 city5)
	(fuel-level plane5 fl1)
	(at person1 city8)
	(at person2 city10)
	(at person3 city7)
	(at person4 city5)
	(at person5 city1)
	(at person6 city10)
	(at person7 city11)
	(at person8 city8)
	(at person9 city9)
	(at person10 city11)
	(at person11 city4)
	(at person12 city5)
	(at person13 city8)
	(at person14 city4)
	(at person15 city1)
	(next fl0 fl1)
	(next fl1 fl2)
	(next fl2 fl3)
	(next fl3 fl4)
	(next fl4 fl5)
	(next fl5 fl6)
)
(:goal
	(and
		(at person1 city1)
		(at person2 city4)
		(at person3 city7)
		(at person4 city6)
		(at person5 city8)
		(at person6 city11)
		(at person7 city2)
		(at person8 city11)
		(at person10 city9)
		(at person11 city6)
		(at person12 city4)
		(at person13 city11)
		(at person14 city4)
		(at person15 city6)
	)
)
)