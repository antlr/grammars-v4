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
	(clear a)
	(clear c)
	(ontable g)
	(ontable f)
	(on a g)
	(on c d)
	(on d b)
	(on b e)
	(on e f)
)
(:goal
	(and
		(on a e)
		(on e b)
		(on b f)
		(on f g)
		(on g c)
		(on c d)
	)
)
)