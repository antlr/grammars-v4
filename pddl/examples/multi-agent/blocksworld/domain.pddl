(define (domain blocks)
	(:requirements :typing :multi-agent :unfactored-privacy)
(:types
	agent block - object
)
(:predicates
	(on ?x - block ?y - block)
	(ontable ?x - block)
	(clear ?x - block)

	(:private ?agent - agent
		(holding ?agent - agent ?x - block)
		(handempty ?agent - agent)
	)
)

(:action pick-up
	:agent ?a - agent
	:parameters (?x - block)
	:precondition (and
		(clear ?x)
		(ontable ?x)
		(handempty ?a)
	)
	:effect (and
		(not (ontable ?x))
		(not (clear ?x))
		(not (handempty ?a))
		(holding ?a ?x)
	)
)


(:action put-down
	:agent ?a - agent
	:parameters (?x - block)
	:precondition 
		(holding ?a ?x)
	:effect (and
		(not (holding ?a ?x))
		(clear ?x)
		(handempty ?a)
		(ontable ?x)
	)
)


(:action stack
	:agent ?a - agent
	:parameters (?x - block ?y - block)
	:precondition (and
		(holding ?a ?x)
		(clear ?y)
	)
	:effect (and
		(not (holding ?a ?x))
		(not (clear ?y))
		(clear ?x)
		(handempty ?a)
		(on ?x ?y)
	)
)


(:action unstack
	:agent ?a - agent
	:parameters (?x - block ?y - block)
	:precondition (and
		(on ?x ?y)
		(clear ?x)
		(handempty ?a)
	)
	:effect (and
		(holding ?a ?x)
		(clear ?y)
		(not (clear ?x))
		(not (handempty ?a))
		(not (on ?x ?y))
	)
)

)