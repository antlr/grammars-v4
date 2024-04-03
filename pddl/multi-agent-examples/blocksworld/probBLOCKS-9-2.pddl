(define (problem BLOCKS-4-0) (:domain blocks)
(:objects
	a - block
	c - block
	b - block
	e - block
	d - block
	g - block
	f - block
	i - block
	h - block

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
	(clear h)
	(clear f)
	(ontable g)
	(ontable f)
	(on h a)
	(on a d)
	(on d e)
	(on e c)
	(on c i)
	(on i b)
	(on b g)
)
(:goal
	(and
		(on f g)
		(on g h)
		(on h d)
		(on d i)
		(on i e)
		(on e b)
		(on b c)
		(on c a)
	)
)
)