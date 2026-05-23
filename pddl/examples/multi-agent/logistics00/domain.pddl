(define (domain logistics)
	(:requirements :typing :multi-agent :unfactored-privacy)
(:types
	location vehicle package city - object
	airport - location
	truck airplane - vehicle
)
(:predicates
	(at ?obj - object ?loc - location)
	(in ?obj1 - package ?veh - vehicle)

	(:private ?agent - truck
		(in-city ?loc - location ?city - city)
	)
)

(:action load-airplane
	:agent ?airplane - airplane
	:parameters (?obj - package ?loc - airport)
	:precondition (and
		(at ?obj ?loc)
		(at ?airplane ?loc)
	)
	:effect (and
		(not (at ?obj ?loc))
		(in ?obj ?airplane)
	)
)


(:action unload-airplane
	:agent ?airplane - airplane
	:parameters (?obj - package ?loc - airport)
	:precondition (and
		(in ?obj ?airplane)
		(at ?airplane ?loc)
	)
	:effect (and
		(not (in ?obj ?airplane))
		(at ?obj ?loc)
	)
)


(:action fly-airplane
	:agent ?airplane - airplane
	:parameters (?loc-from - airport ?loc-to - airport)
	:precondition 
		(at ?airplane ?loc-from)
	:effect (and
		(not (at ?airplane ?loc-from))
		(at ?airplane ?loc-to)
	)
)


(:action load-truck
	:agent ?truck - truck
	:parameters (?obj - package ?loc - location)
	:precondition (and
		(at ?truck ?loc)
		(at ?obj ?loc)
	)
	:effect (and
		(not (at ?obj ?loc))
		(in ?obj ?truck)
	)
)


(:action unload-truck
	:agent ?truck - truck
	:parameters (?obj - package ?loc - location)
	:precondition (and
		(at ?truck ?loc)
		(in ?obj ?truck)
	)
	:effect (and
		(not (in ?obj ?truck))
		(at ?obj ?loc)
	)
)


(:action drive-truck
	:agent ?truck - truck
	:parameters (?loc-from - location ?loc-to - location ?city - city)
	:precondition (and
		(at ?truck ?loc-from)
		(in-city ?loc-from ?city)
		(in-city ?loc-to ?city)
	)
	:effect (and
		(not (at ?truck ?loc-from))
		(at ?truck ?loc-to)
	)
)

)