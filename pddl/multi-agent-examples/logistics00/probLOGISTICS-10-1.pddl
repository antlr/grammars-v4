(define (problem logistics-10-1) (:domain logistics)
(:objects
	apt3 - airport
	apt2 - airport
	apt1 - airport
	apt4 - airport
	pos4 - location
	pos3 - location
	pos1 - location
	obj21 - package
	obj22 - package
	obj23 - package
	obj33 - package
	obj32 - package
	obj31 - package
	obj42 - package
	obj43 - package
	obj41 - package
	obj11 - package
	obj13 - package
	obj12 - package

	(:private apn1
		apn1 - airplane
	)

	(:private tru4
		cit4 - city
		tru4 - truck
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
	(at tru4 pos4)
	(at obj41 pos4)
	(at obj42 pos4)
	(at obj43 pos4)
	(in-city pos1 cit1)
	(in-city apt1 cit1)
	(in-city pos2 cit2)
	(in-city apt2 cit2)
	(in-city pos3 cit3)
	(in-city apt3 cit3)
	(in-city pos4 cit4)
	(in-city apt4 cit4)
)
(:goal
	(and
		(at obj43 apt4)
		(at obj32 pos3)
		(at obj42 apt3)
		(at obj12 pos1)
		(at obj41 apt3)
		(at obj23 pos3)
		(at obj13 apt4)
		(at obj22 pos4)
		(at obj31 apt3)
		(at obj33 apt1)
	)
)
)