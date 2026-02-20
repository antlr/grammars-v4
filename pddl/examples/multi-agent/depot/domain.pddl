(define (domain depot)
	(:requirements :typing :multi-agent :unfactored-privacy)
(:types
	place locatable driver - object
	depot distributor - place
	truck hoist surface - locatable
	pallet crate - surface
)
(:predicates
	(at ?x - locatable ?y - place)
	(on ?x - crate ?y - surface)
	(in ?x - crate ?y - truck)
	(clear ?x - surface)

	(:private ?agent - place
		(lifting ?agent - place ?x - hoist ?y - crate)
		(available ?agent - place ?x - hoist)
	)

	(:private ?agent - driver
		(driving ?agent - driver ?t - truck)
	)

)

(:action drive
	:agent ?a - driver
	:parameters (?x - truck ?y - place ?z - place)
	:precondition 
		(and (at ?x ?y) (driving ?a ?x))
	:effect (and
		(at ?x ?z)
		(not (at ?x ?y))
	)
)


(:action lift
	:agent ?p - place
	:parameters (?x - hoist ?y - crate ?z - surface)
	:precondition (and
		(at ?x ?p)
		(available ?p ?x)
		(at ?y ?p)
		(on ?y ?z)
		(clear ?y)
	)
	:effect (and
		(lifting ?p ?x ?y)
		(clear ?z)
		(not (at ?y ?p))
		(not (clear ?y))
		(not (available ?p ?x))
		(not (on ?y ?z))
	)
)


(:action drop
	:agent ?p - place
	:parameters (?x - hoist ?y - crate ?z - surface)
	:precondition (and
		(at ?x ?p)
		(at ?z ?p)
		(clear ?z)
		(lifting ?p ?x ?y)
	)
	:effect (and
		(available ?p ?x)
		(at ?y ?p)
		(clear ?y)
		(on ?y ?z)
		(not (lifting ?p ?x ?y))
		(not (clear ?z))
	)
)


(:action load
	:agent ?p - place
	:parameters (?x - hoist ?y - crate ?z - truck)
	:precondition (and
		(at ?x ?p)
		(at ?z ?p)
		(lifting ?p ?x ?y)
	)
	:effect (and
		(in ?y ?z)
		(available ?p ?x)
		(not (lifting ?p ?x ?y))
	)
)


(:action unload
	:agent ?p - place
	:parameters (?x - hoist ?y - crate ?z - truck)
	:precondition (and
		(at ?x ?p)
		(at ?z ?p)
		(available ?p ?x)
		(in ?y ?z)
	)
	:effect (and
		(lifting ?p ?x ?y)
		(not (in ?y ?z))
		(not (available ?p ?x))
	)
)

)
