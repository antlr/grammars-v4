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
	j - block

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
	(clear c)
	(clear f)
	(ontable i)
	(ontable f)
	(on c e)
	(on e j)
	(on j b)
	(on b g)
	(on g h)
	(on h a)
	(on a d)
	(on d i)
)
(:goal
	(and
		(on d c)
		(on c f)
		(on f j)
		(on j e)
		(on e h)
		(on h b)
		(on b a)
		(on a g)
		(on g i)
	)
)
)