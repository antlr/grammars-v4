(define (problem BLOCKS-4-0) (:domain blocks)
(:objects
	a - block
	c - block
	b - block
	e - block
	d - block
	g - block
	f - block
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
	(clear e)
	(clear h)
	(clear d)
	(clear f)
	(ontable c)
	(ontable g)
	(ontable d)
	(ontable f)
	(on e c)
	(on h a)
	(on a b)
	(on b g)
)
(:goal
	(and
		(on c d)
		(on d b)
		(on b g)
		(on g f)
		(on f h)
		(on h a)
		(on a e)
	)
)
)