(define (problem ZTRAVEL-5-25) (:domain zeno-travel)
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
	city1 - city
	city6 - city
	person8 - person
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
	plane1 - aircraft
	plane2 - aircraft
	person25 - person
	person24 - person
	person21 - person
	person20 - person
	person23 - person
	person22 - person
	city7 - city
	city18 - city
	city19 - city
	city14 - city
	city15 - city
	city16 - city
	city17 - city
	city10 - city
	city11 - city
	city12 - city
	city13 - city
	person1 - person

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
	(fuel-level plane1 fl1)
	(at plane2 city1)
	(fuel-level plane2 fl1)
	(at plane3 city16)
	(fuel-level plane3 fl5)
	(at plane4 city10)
	(fuel-level plane4 fl2)
	(at plane5 city1)
	(fuel-level plane5 fl1)
	(at person1 city19)
	(at person2 city3)
	(at person3 city6)
	(at person4 city16)
	(at person5 city13)
	(at person6 city6)
	(at person7 city0)
	(at person8 city3)
	(at person9 city13)
	(at person10 city16)
	(at person11 city2)
	(at person12 city6)
	(at person13 city8)
	(at person14 city0)
	(at person15 city8)
	(at person16 city19)
	(at person17 city9)
	(at person18 city0)
	(at person19 city1)
	(at person20 city12)
	(at person21 city8)
	(at person22 city15)
	(at person23 city4)
	(at person24 city6)
	(at person25 city17)
	(next fl0 fl1)
	(next fl1 fl2)
	(next fl2 fl3)
	(next fl3 fl4)
	(next fl4 fl5)
	(next fl5 fl6)
)
(:goal
	(and
		(at plane1 city11)
		(at plane2 city8)
		(at person1 city10)
		(at person2 city1)
		(at person3 city13)
		(at person4 city9)
		(at person5 city0)
		(at person6 city16)
		(at person7 city0)
		(at person8 city0)
		(at person9 city17)
		(at person10 city13)
		(at person11 city13)
		(at person12 city17)
		(at person13 city3)
		(at person14 city0)
		(at person15 city13)
		(at person16 city19)
		(at person17 city0)
		(at person18 city4)
		(at person19 city17)
		(at person20 city14)
		(at person21 city17)
		(at person22 city4)
		(at person23 city12)
		(at person24 city13)
		(at person25 city2)
	)
)
)