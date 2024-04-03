(define (domain driverlog)
	(:requirements :typing :multi-agent :unfactored-privacy)
(:types
	location locatable - object
	driver truck package - locatable
)
(:predicates
	(in ?obj1 - package ?obj - truck)
	(path ?x - location ?y - location)
	(empty ?v - truck)
	(at ?obj - locatable ?loc - location)
	(link ?x - location ?y - location)

	(:private ?agent - driver
		(driving ?agent - driver ?v - truck)
	)
)

(:action LOAD-TRUCK
	:agent ?driver - driver
	:parameters (?truck - truck ?obj - package ?loc - location)
	:precondition (and
		(at ?truck ?loc)
		(at ?obj ?loc)
		(driving ?driver ?truck)
	)
	:effect (and
		(not (at ?obj ?loc))
		(in ?obj ?truck)
	)
)


(:action UNLOAD-TRUCK
	:agent ?driver - driver
	:parameters (?truck - truck ?obj - package ?loc - location)
	:precondition (and
		(at ?truck ?loc)
		(in ?obj ?truck)
		(driving ?driver ?truck)
	)
	:effect (and
		(not (in ?obj ?truck))
		(at ?obj ?loc)
	)
)


(:action BOARD-TRUCK
	:agent ?driver - driver
	:parameters (?truck - truck ?loc - location)
	:precondition (and
		(at ?truck ?loc)
		(at ?driver ?loc)
		(empty ?truck)
	)
	:effect (and
		(not (at ?driver ?loc))
		(driving ?driver ?truck)
		(not (empty ?truck))
	)
)


(:action DISEMBARK-TRUCK
	:agent ?driver - driver
	:parameters (?truck - truck ?loc - location)
	:precondition (and
		(at ?truck ?loc)
		(driving ?driver ?truck)
	)
	:effect (and
		(not (driving ?driver ?truck))
		(at ?driver ?loc)
		(empty ?truck)
	)
)


(:action DRIVE-TRUCK
	:agent ?driver - driver
	:parameters (?loc-from - location ?loc-to - location ?truck - truck)
	:precondition (and
		(at ?truck ?loc-from)
		(driving ?driver ?truck)
		(link ?loc-from ?loc-to)
	)
	:effect (and
		(not (at ?truck ?loc-from))
		(at ?truck ?loc-to)
	)
)


(:action WALK
	:agent ?driver - driver
	:parameters (?loc-from - location ?loc-to - location)
	:precondition (and
		(at ?driver ?loc-from)
		(path ?loc-from ?loc-to)
	)
	:effect (and
		(not (at ?driver ?loc-from))
		(at ?driver ?loc-to)
	)
)

)