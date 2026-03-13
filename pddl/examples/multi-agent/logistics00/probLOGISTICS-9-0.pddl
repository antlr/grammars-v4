(define (problem logistics-9-0) (:domain logistics)
(:objects
	obj21 - package
	obj22 - package
	obj23 - package
	obj33 - package
	obj32 - package
	obj31 - package
	apt3 - airport
	apt2 - airport
	apt1 - airport
	obj11 - package
	obj13 - package
	obj12 - package
	pos3 - location
	pos1 - location

	(:private apn1
		apn1 - airplane
	)

	(:private tru3
		tru3 - truck
		cit3 - city
	)

	(:private tru2
		cit2 - city
		tru2 - truck
		pos2 - location
	)

	(:private tru1
		tru1 - truck
		cit1 - city
	)
)
(:init
	(at apn1 apt2)
	(at tru1 pos1)
	(at obj11 pos1)
	(at obj12 pos1)
	(at obj13 pos1)
	(at tru2 pos2)
	(at obj21 pos2)
	(at obj22 pos2)
	(at obj23 pos2)
	(at tru3 pos3)
	(at obj31 pos3)
	(at obj32 pos3)
	(at obj33 pos3)
	(in-city pos1 cit1)
	(in-city apt1 cit1)
	(in-city pos2 cit2)
	(in-city apt2 cit2)
	(in-city pos3 cit3)
	(in-city apt3 cit3)
)
(:goal
	(and
		(at obj23 pos3)
		(at obj32 pos1)
		(at obj22 pos1)
		(at obj31 apt3)
		(at obj11 pos1)
		(at obj33 pos3)
		(at obj13 apt3)
		(at obj12 pos1)
		(at obj21 apt1)
	)
)
)