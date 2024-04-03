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
	o - block
	n - block

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
	(clear m)
	(clear b)
	(clear f)
	(clear i)
	(ontable g)
	(ontable n)
	(ontable o)
	(ontable k)
	(ontable h)
	(on e j)
	(on j d)
	(on d l)
	(on l c)
	(on c g)
	(on m n)
	(on b a)
	(on a o)
	(on f k)
	(on i h)
)
(:goal
	(and
		(on g o)
		(on o h)
		(on h k)
		(on k m)
		(on m f)
		(on f e)
		(on e a)
		(on a b)
		(on b l)
		(on l j)
		(on j d)
		(on d n)
		(on n i)
		(on i c)
	)
)
)