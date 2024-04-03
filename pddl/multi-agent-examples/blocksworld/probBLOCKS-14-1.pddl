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
	(clear g)
	(clear c)
	(clear i)
	(clear h)
	(clear n)
	(ontable j)
	(ontable e)
	(ontable m)
	(ontable b)
	(ontable n)
	(on g j)
	(on c e)
	(on i d)
	(on d l)
	(on l m)
	(on h f)
	(on f a)
	(on a k)
	(on k b)
)
(:goal
	(and
		(on j d)
		(on d b)
		(on b h)
		(on h m)
		(on m k)
		(on k f)
		(on f g)
		(on g a)
		(on a i)
		(on i e)
		(on e l)
		(on l n)
		(on n c)
	)
)
)