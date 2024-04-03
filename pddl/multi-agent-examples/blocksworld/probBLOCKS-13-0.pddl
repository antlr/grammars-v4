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
	(clear b)
	(clear i)
	(clear m)
	(ontable k)
	(ontable g)
	(ontable m)
	(on b f)
	(on f d)
	(on d c)
	(on c j)
	(on j a)
	(on a e)
	(on e h)
	(on h l)
	(on l k)
	(on i g)
)
(:goal
	(and
		(on g i)
		(on i c)
		(on c d)
		(on d f)
		(on f a)
		(on a m)
		(on m h)
		(on h e)
		(on e l)
		(on l j)
		(on j b)
		(on b k)
	)
)
)