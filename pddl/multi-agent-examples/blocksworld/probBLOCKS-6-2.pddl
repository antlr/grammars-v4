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
	(ontable c)
	(on a d)
	(on d b)
	(on b f)
	(on f e)
	(on e c)
)
(:goal
	(and
		(on e f)
		(on f a)
		(on a b)
		(on b c)
		(on c d)
	)
)
)