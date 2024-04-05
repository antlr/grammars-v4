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
	p - block

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
	(clear f)
	(clear h)
	(clear o)
	(ontable a)
	(ontable e)
	(ontable g)
	(on f l)
	(on l m)
	(on m j)
	(on j p)
	(on p n)
	(on n i)
	(on i b)
	(on b d)
	(on d c)
	(on c k)
	(on k a)
	(on h e)
	(on o g)
)
(:goal
	(and
		(on d b)
		(on b p)
		(on p f)
		(on f g)
		(on g k)
		(on k i)
		(on i l)
		(on l j)
		(on j h)
		(on h a)
		(on a n)
		(on n e)
		(on e m)
		(on m c)
		(on c o)
	)
)
)