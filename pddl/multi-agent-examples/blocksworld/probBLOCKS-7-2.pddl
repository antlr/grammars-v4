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
	(clear b)
	(clear a)
	(ontable f)
	(ontable d)
	(on b c)
	(on c g)
	(on g e)
	(on e f)
	(on a d)
)
(:goal
	(and
		(on e b)
		(on b f)
		(on f d)
		(on d a)
		(on a c)
		(on c g)
	)
)
)