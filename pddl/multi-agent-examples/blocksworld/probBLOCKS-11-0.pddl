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
	(clear b)
	(clear j)
	(clear c)
	(ontable i)
	(ontable d)
	(ontable e)
	(on b g)
	(on g h)
	(on h k)
	(on k a)
	(on a f)
	(on f i)
	(on j d)
	(on c e)
)
(:goal
	(and
		(on a j)
		(on j d)
		(on d b)
		(on b h)
		(on h k)
		(on k i)
		(on i f)
		(on f e)
		(on e g)
		(on g c)
	)
)
)