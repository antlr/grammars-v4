(define (problem BLOCKS-4-0) (:domain blocks)
(:objects
	a - block
	c - block
	b - block
	e - block
	d - block
	g - block
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
	(clear e)
	(ontable d)
	(on e g)
	(on g b)
	(on b a)
	(on a f)
	(on f c)
	(on c d)
)
(:goal
	(and
		(on a g)
		(on g d)
		(on d b)
		(on b c)
		(on c f)
		(on f e)
	)
)
)