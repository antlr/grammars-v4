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
	(clear j)
	(clear c)
	(ontable a)
	(ontable c)
	(on j i)
	(on i h)
	(on h f)
	(on f d)
	(on d e)
	(on e g)
	(on g b)
	(on b a)
)
(:goal
	(and
		(on b e)
		(on e i)
		(on i g)
		(on g h)
		(on h c)
		(on c a)
		(on a f)
		(on f j)
		(on j d)
	)
)
)