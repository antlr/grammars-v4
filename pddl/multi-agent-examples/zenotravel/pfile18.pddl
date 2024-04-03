(define (problem ZTRAVEL-5-20) (:domain zeno-travel)
(:objects
	person14 - person
	person15 - person
	person16 - person
	person17 - person
	person10 - person
	person11 - person
	person12 - person
	person13 - person
	person18 - person
	person19 - person
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
	city0 - city
	person9 - person
	city6 - city
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
	plane2 - aircraft
	person20 - person
	city7 - city
	city14 - city
	city15 - city
	city16 - city
	city17 - city
	city10 - city
	city11 - city
	city12 - city
	city13 - city
	person1 - person

	(:private plane1
		plane1 - aircraft
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
	(at plane1 city11)
	(fuel-level plane1 fl3)
	(at plane2 city12)
	(fuel-level plane2 fl3)
	(at plane3 city12)
	(fuel-level plane3 fl0)
	(at plane4 city14)
	(fuel-level plane4 fl0)
	(at plane5 city14)
	(fuel-level plane5 fl6)
	(at person1 city7)
	(at person2 city16)
	(at person3 city9)
	(at person4 city6)
	(at person5 city6)
	(at person6 city0)
	(at person7 city6)
	(at person8 city8)
	(at person9 city13)
	(at person10 city14)
	(at person11 city7)
	(at person12 city15)
	(at person13 city4)
	(at person14 city14)
	(at person15 city10)
	(at person16 city5)
	(at person17 city10)
	(at person18 city11)
	(at person19 city9)
	(at person20 city3)
	(next fl0 fl1)
	(next fl1 fl2)
	(next fl2 fl3)
	(next fl3 fl4)
	(next fl4 fl5)
	(next fl5 fl6)
)
(:goal
	(and
		(at plane2 city6)
		(at person1 city2)
		(at person2 city17)
		(at person3 city7)
		(at person4 city14)
		(at person5 city9)
		(at person6 city13)
		(at person7 city6)
		(at person8 city8)
		(at person9 city4)
		(at person10 city16)
		(at person11 city6)
		(at person12 city4)
		(at person13 city14)
		(at person14 city2)
		(at person15 city9)
		(at person16 city9)
		(at person17 city15)
		(at person18 city6)
		(at person19 city2)
		(at person20 city17)
	)
)
)