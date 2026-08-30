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
	person1 - person
	person6 - person
	person7 - person
	person4 - person
	person5 - person
	city2 - city
	city3 - city
	person8 - person
	person9 - person
	city6 - city
	city0 - city
	city4 - city
	city5 - city
	city21 - city
	city20 - city
	city1 - city
	fl1 - flevel
	fl0 - flevel
	fl3 - flevel
	fl2 - flevel
	fl5 - flevel
	fl4 - flevel
	fl6 - flevel
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
	city9 - city

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
	(fuel-level plane1 fl6)
	(at plane2 city6)
	(fuel-level plane2 fl0)
	(at plane3 city18)
	(fuel-level plane3 fl0)
	(at plane4 city11)
	(fuel-level plane4 fl6)
	(at plane5 city9)
	(fuel-level plane5 fl4)
	(at person1 city12)
	(at person2 city13)
	(at person3 city12)
	(at person4 city1)
	(at person5 city20)
	(at person6 city13)
	(at person7 city13)
	(at person8 city4)
	(at person9 city7)
	(at person10 city7)
	(at person11 city8)
	(at person12 city14)
	(at person13 city1)
	(at person14 city14)
	(at person15 city2)
	(at person16 city21)
	(at person17 city8)
	(at person18 city4)
	(at person19 city8)
	(at person20 city17)
	(at person21 city5)
	(at person22 city21)
	(at person23 city15)
	(at person24 city6)
	(at person25 city5)
	(next fl0 fl1)
	(next fl1 fl2)
	(next fl2 fl3)
	(next fl3 fl4)
	(next fl4 fl5)
	(next fl5 fl6)
)
(:goal
	(and
		(at person1 city5)
		(at person2 city0)
		(at person3 city18)
		(at person4 city7)
		(at person5 city8)
		(at person6 city4)
		(at person7 city12)
		(at person8 city16)
		(at person9 city20)
		(at person10 city5)
		(at person11 city18)
		(at person12 city10)
		(at person13 city0)
		(at person14 city1)
		(at person15 city0)
		(at person16 city13)
		(at person17 city4)
		(at person18 city9)
		(at person19 city16)
		(at person20 city1)
		(at person21 city10)
		(at person22 city2)
		(at person23 city4)
		(at person24 city18)
		(at person25 city21)
	)
)
)