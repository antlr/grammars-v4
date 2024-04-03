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
	k - block
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
	(clear f)
	(clear d)
	(clear g)
	(ontable i)
	(ontable k)
	(ontable h)
	(ontable a)
	(on j i)
	(on f e)
	(on e k)
	(on d c)
	(on c h)
	(on g b)
	(on b a)
)
(:goal
	(and
		(on b d)
		(on d j)
		(on j k)
		(on k h)
		(on h a)
		(on a c)
		(on c f)
		(on f g)
		(on g i)
		(on i e)
	)
)
)