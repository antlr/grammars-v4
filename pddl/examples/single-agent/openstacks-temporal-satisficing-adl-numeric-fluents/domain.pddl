(define (domain openstacks-time-numeric-ADL)
(:requirements :typing :adl :durative-actions :numeric-fluents)
(:types order product)

(:predicates 
	(includes ?o - order ?p - product)
	(waiting ?o - order)
	(started ?o - order)
	(shipped ?o - order)
	(made ?p - product)
	(not-made ?p - product)
)

(:functions
(stacks-in-use) 
(max-stacks) 
(make-time ?p - product) 
)

(:durative-action start-order
:parameters (?o - order)
:duration (= ?duration 1)
:condition (and (at start (waiting ?o))(at start (< (stacks-in-use) (max-stacks))))
:effect (and (at start (not (waiting ?o)))(at end (started ?o))(at start (increase (stacks-in-use) 1)))
)

  (:durative-action make-product
    :parameters (?p - product)
    :duration (= ?duration (make-time ?p))
    :condition (and (at start (not (made ?p)))
		    (at start (forall (?o - order)
				 (imply (includes ?o ?p) (started ?o)))))
    :effect (at end (made ?p))
    )

(:durative-action ship-order
    :parameters (?o - order)
    :duration (= ?duration 1)
    :condition (and (at start (started ?o))
		    (at start
			(forall (?p - product)
				(imply (includes ?o ?p) (made ?p)))))
    :effect (and (at start (not (started ?o)))
		 (at end (shipped ?o))
		 (at end (decrease (stacks-in-use) 1)))
    )
    
)

