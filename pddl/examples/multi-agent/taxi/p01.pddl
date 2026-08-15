(define (problem taxi-01)

	(:domain taxi)
	
	(:objects
		t1 t2 - taxi
		p1 p2 - passenger
		g1 g2 c h1 h2 - location
	)
		
	(:init
		(directly-connected g1 c)
		(directly-connected g2 c)
		(directly-connected c g1)
		(directly-connected c g2)
		(directly-connected c h1)
		(directly-connected c h2)
		(directly-connected h1 c)
		(directly-connected h2 c)
		(at t1 g1)
		(at t2 g2)
		(empty t1)
		(empty t2)
		(at p1 h1)
		(at p2 h2)
		(free h1)
		(free h2)
		(free c)
		(goal-of p1 c)
		(goal-of p2 c)
	)
	
	(:goal 	
		(and	(at t1 g1)
				(at t2 g2)
				(at p1 c)
				(at p2 c)
		)
	)
)
