(define (domain woodworking)
	(:requirements :typing :multi-agent :unfactored-privacy)
(:types
	acolour awood woodobj machine surface treatmentstatus aboardsize apartsize - object
	highspeed-saw saw glazer grinder immersion-varnisher planer spray-varnisher - machine
	board part - woodobj
)
(:constants
	natural  - acolour
	small medium large  - apartsize
	varnished glazed untreated colourfragments  - treatmentstatus
	verysmooth smooth rough  - surface
)
(:predicates
	(available ?obj - woodobj)
	(surface-condition ?obj - woodobj ?surface - surface)
	(treatment ?obj - part ?treatment - treatmentstatus)
	(colour ?obj - part ?colour - acolour)
	(wood ?obj - woodobj ?wood - awood)
	(is-smooth ?surface - surface)
	(has-colour ?agent - machine ?colour - acolour)
	(goalsize ?part - part ?size - apartsize)
	(boardsize-successor ?size1 - aboardsize ?size2 - aboardsize)
	(unused ?obj - part)
	(boardsize ?board - board ?size - aboardsize)

	(:private ?agent - grinder
		(grind-treatment-change ?agent - grinder ?old - treatmentstatus ?new - treatmentstatus)
	)

	(:private ?agent - highspeed-saw
		(empty ?agent - highspeed-saw)
		(in-highspeed-saw ?b - board ?agent - highspeed-saw)
	)

)
(:functions
	(total-cost) - number
	(spray-varnish-cost ?obj - part) - number
	(glaze-cost ?obj - part) - number
	(grind-cost ?obj - part) - number
	(plane-cost ?obj - part) - number
)

(:action do-immersion-varnish
	:agent ?m - immersion-varnisher
	:parameters (?x - part ?newcolour - acolour ?surface - surface)
	:precondition (and
		(available ?x)
		(has-colour ?m ?newcolour)
		(surface-condition ?x ?surface)
		(is-smooth ?surface)
		(treatment ?x untreated)
	)
	:effect (and
		(increase ( total-cost ) 10)
		(not (treatment ?x untreated))
		(treatment ?x varnished)
		(not (colour ?x natural))
		(colour ?x ?newcolour)
	)
)


(:action do-spray-varnish
	:agent ?m - spray-varnisher
	:parameters (?x - part ?newcolour - acolour ?surface - surface)
	:precondition (and
		(available ?x)
		(has-colour ?m ?newcolour)
		(surface-condition ?x ?surface)
		(is-smooth ?surface)
		(treatment ?x untreated)
	)
	:effect (and
		(increase ( total-cost ) ( spray-varnish-cost ?x ))
		(not (treatment ?x untreated))
		(treatment ?x varnished)
		(not (colour ?x natural))
		(colour ?x ?newcolour)
	)
)


(:action do-glaze
	:agent ?m - glazer
	:parameters (?x - part ?newcolour - acolour)
	:precondition (and
		(available ?x)
		(has-colour ?m ?newcolour)
		(treatment ?x untreated)
	)
	:effect (and
		(increase ( total-cost ) ( glaze-cost ?x ))
		(not (treatment ?x untreated))
		(treatment ?x glazed)
		(not (colour ?x natural))
		(colour ?x ?newcolour)
	)
)


(:action do-grind
	:agent ?m - grinder
	:parameters (?x - part ?oldsurface - surface ?oldcolour - acolour ?oldtreatment - treatmentstatus ?newtreatment - treatmentstatus)
	:precondition (and
		(available ?x)
		(surface-condition ?x ?oldsurface)
		(is-smooth ?oldsurface)
		(colour ?x ?oldcolour)
		(treatment ?x ?oldtreatment)
		(grind-treatment-change ?m ?oldtreatment ?newtreatment)
	)
	:effect (and
		(increase ( total-cost ) ( grind-cost ?x ))
		(not (surface-condition ?x ?oldsurface))
		(surface-condition ?x verysmooth)
		(not (treatment ?x ?oldtreatment))
		(treatment ?x ?newtreatment)
		(not (colour ?x ?oldcolour))
		(colour ?x natural)
	)
)


(:action do-plane
	:agent ?m - planer
	:parameters (?x - part ?oldsurface - surface ?oldcolour - acolour ?oldtreatment - treatmentstatus)
	:precondition (and
		(available ?x)
		(surface-condition ?x ?oldsurface)
		(treatment ?x ?oldtreatment)
		(colour ?x ?oldcolour)
	)
	:effect (and
		(increase ( total-cost ) ( plane-cost ?x ))
		(not (surface-condition ?x ?oldsurface))
		(surface-condition ?x smooth)
		(not (treatment ?x ?oldtreatment))
		(treatment ?x untreated)
		(not (colour ?x ?oldcolour))
		(colour ?x natural)
	)
)


(:action load-highspeed-saw
	:agent ?m - highspeed-saw
	:parameters (?b - board)
	:precondition (and
		(empty ?m)
		(available ?b)
	)
	:effect (and
		(increase ( total-cost ) 30)
		(not (available ?b))
		(not (empty ?m))
		(in-highspeed-saw ?b ?m)
	)
)


(:action unload-highspeed-saw
	:agent ?m - highspeed-saw
	:parameters (?b - board)
	:precondition 
		(in-highspeed-saw ?b ?m)
	:effect (and
		(increase ( total-cost ) 10)
		(available ?b)
		(not (in-highspeed-saw ?b ?m))
		(empty ?m)
	)
)


(:action cut-board-small
	:agent ?m - highspeed-saw
	:parameters (?b - board ?p - part ?w - awood ?surface - surface ?size_before - aboardsize ?size_after - aboardsize)
	:precondition (and
		(unused ?p)
		(goalsize ?p small)
		(in-highspeed-saw ?b ?m)
		(wood ?b ?w)
		(surface-condition ?b ?surface)
		(boardsize ?b ?size_before)
		(boardsize-successor ?size_after ?size_before)
	)
	:effect (and
		(increase ( total-cost ) 10)
		(not (unused ?p))
		(available ?p)
		(wood ?p ?w)
		(surface-condition ?p ?surface)
		(colour ?p natural)
		(treatment ?p untreated)
		(boardsize ?b ?size_after)
	)
)


(:action cut-board-medium
	:agent ?m - highspeed-saw
	:parameters (?b - board ?p - part ?w - awood ?surface - surface ?size_before - aboardsize ?s1 - aboardsize ?size_after - aboardsize)
	:precondition (and
		(unused ?p)
		(goalsize ?p medium)
		(in-highspeed-saw ?b ?m)
		(wood ?b ?w)
		(surface-condition ?b ?surface)
		(boardsize ?b ?size_before)
		(boardsize-successor ?size_after ?s1)
		(boardsize-successor ?s1 ?size_before)
	)
	:effect (and
		(increase ( total-cost ) 10)
		(not (unused ?p))
		(available ?p)
		(wood ?p ?w)
		(surface-condition ?p ?surface)
		(colour ?p natural)
		(treatment ?p untreated)
		(boardsize ?b ?size_after)
	)
)


(:action cut-board-large
	:agent ?m - highspeed-saw
	:parameters (?b - board ?p - part ?w - awood ?surface - surface ?size_before - aboardsize ?s1 - aboardsize ?s2 - aboardsize ?size_after - aboardsize)
	:precondition (and
		(unused ?p)
		(goalsize ?p large)
		(in-highspeed-saw ?b ?m)
		(wood ?b ?w)
		(surface-condition ?b ?surface)
		(boardsize ?b ?size_before)
		(boardsize-successor ?size_after ?s1)
		(boardsize-successor ?s1 ?s2)
		(boardsize-successor ?s2 ?size_before)
	)
	:effect (and
		(increase ( total-cost ) 10)
		(not (unused ?p))
		(available ?p)
		(wood ?p ?w)
		(surface-condition ?p ?surface)
		(colour ?p natural)
		(treatment ?p untreated)
		(boardsize ?b ?size_after)
	)
)


(:action do-saw-small
	:agent ?m - saw
	:parameters (?b - board ?p - part ?w - awood ?surface - surface ?size_before - aboardsize ?size_after - aboardsize)
	:precondition (and
		(unused ?p)
		(goalsize ?p small)
		(available ?b)
		(wood ?b ?w)
		(surface-condition ?b ?surface)
		(boardsize ?b ?size_before)
		(boardsize-successor ?size_after ?size_before)
	)
	:effect (and
		(increase ( total-cost ) 30)
		(not (unused ?p))
		(available ?p)
		(wood ?p ?w)
		(surface-condition ?p ?surface)
		(colour ?p natural)
		(treatment ?p untreated)
		(boardsize ?b ?size_after)
	)
)


(:action do-saw-medium
	:agent ?m - saw
	:parameters (?b - board ?p - part ?w - awood ?surface - surface ?size_before - aboardsize ?s1 - aboardsize ?size_after - aboardsize)
	:precondition (and
		(unused ?p)
		(goalsize ?p medium)
		(available ?b)
		(wood ?b ?w)
		(surface-condition ?b ?surface)
		(boardsize ?b ?size_before)
		(boardsize-successor ?size_after ?s1)
		(boardsize-successor ?s1 ?size_before)
	)
	:effect (and
		(increase ( total-cost ) 30)
		(not (unused ?p))
		(available ?p)
		(wood ?p ?w)
		(surface-condition ?p ?surface)
		(colour ?p natural)
		(treatment ?p untreated)
		(boardsize ?b ?size_after)
	)
)


(:action do-saw-large
	:agent ?m - saw
	:parameters (?b - board ?p - part ?w - awood ?surface - surface ?size_before - aboardsize ?s1 - aboardsize ?s2 - aboardsize ?size_after - aboardsize)
	:precondition (and
		(unused ?p)
		(goalsize ?p large)
		(available ?b)
		(wood ?b ?w)
		(surface-condition ?b ?surface)
		(boardsize ?b ?size_before)
		(boardsize-successor ?size_after ?s1)
		(boardsize-successor ?s1 ?s2)
		(boardsize-successor ?s2 ?size_before)
	)
	:effect (and
		(increase ( total-cost ) 30)
		(not (unused ?p))
		(available ?p)
		(wood ?p ?w)
		(surface-condition ?p ?surface)
		(colour ?p natural)
		(treatment ?p untreated)
		(boardsize ?b ?size_after)
	)
)

)
