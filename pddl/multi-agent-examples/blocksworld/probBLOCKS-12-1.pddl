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
	l - block

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
	(clear k)
	(ontable j)
	(ontable d)
	(on c g)
	(on g h)
	(on h i)
	(on i f)
	(on f b)
	(on b a)
	(on a l)
	(on l e)
	(on e j)
	(on k d)
)
(:goal
	(and
		(on j c)
		(on c e)
		(on e k)
		(on k h)
		(on h a)
		(on a f)
		(on f l)
		(on l g)
		(on g b)
		(on b i)
		(on i d)
	)
)
)