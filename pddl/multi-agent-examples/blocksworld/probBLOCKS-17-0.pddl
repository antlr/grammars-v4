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
	q - block
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
	(clear q)
	(clear l)
	(clear g)
	(clear h)
	(clear p)
	(ontable m)
	(ontable k)
	(ontable o)
	(ontable n)
	(ontable p)
	(on q a)
	(on a j)
	(on j i)
	(on i b)
	(on b m)
	(on l f)
	(on f e)
	(on e k)
	(on g d)
	(on d c)
	(on c o)
	(on h n)
)
(:goal
	(and
		(on q n)
		(on n l)
		(on l o)
		(on o j)
		(on j h)
		(on h c)
		(on c e)
		(on e m)
		(on m p)
		(on p a)
		(on a g)
		(on g b)
		(on b i)
		(on i k)
		(on k f)
		(on f d)
	)
)
)