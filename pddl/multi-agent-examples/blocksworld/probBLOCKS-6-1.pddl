(define (problem BLOCKS-4-0) (:domain blocks)
(:objects
	a - block
	c - block
	b - block
	e - block
	d - block
	f - block

	(:private a1
		a1 - agent
	)

	(:private a2
		a2 - agent
	)

	(:private a3
		a3 - agent
	)

	(:private a4
		a4 - agent
	)
)
(:init
	(handempty a1)
	(handempty a2)
	(handempty a3)
	(handempty a4)
	(clear a)
	(clear b)
	(clear e)
	(clear c)
	(clear d)
	(ontable f)
	(ontable b)
	(ontable e)
	(ontable c)
	(ontable d)
	(on a f)
)
(:goal
	(and
		(on e f)
		(on f c)
		(on c b)
		(on b a)
		(on a d)
	)
)
)