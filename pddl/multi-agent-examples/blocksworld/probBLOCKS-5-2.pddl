(define (problem BLOCKS-4-0) (:domain blocks)
(:objects
	a - block
	c - block
	b - block
	e - block
	d - block

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
	(clear d)
	(ontable b)
	(on d e)
	(on e c)
	(on c a)
	(on a b)
)
(:goal
	(and
		(on d c)
		(on c b)
		(on b e)
		(on e a)
	)
)
)