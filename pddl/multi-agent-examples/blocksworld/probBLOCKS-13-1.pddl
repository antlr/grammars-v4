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
	m - block
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
	(clear j)
	(clear b)
	(ontable f)
	(ontable k)
	(on j e)
	(on e d)
	(on d c)
	(on c a)
	(on a l)
	(on l h)
	(on h g)
	(on g m)
	(on m i)
	(on i f)
	(on b k)
)
(:goal
	(and
		(on d a)
		(on a e)
		(on e l)
		(on l m)
		(on m c)
		(on c j)
		(on j f)
		(on f k)
		(on k g)
		(on g h)
		(on h i)
		(on i b)
	)
)
)