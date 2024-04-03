(define (problem BLOCKS-4-0) (:domain blocks)
(:objects
	a - block
	c - block
	b - block
	e - block
	d - block
	g - block
	f - block
	h - block

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
	(clear d)
	(clear a)
	(clear e)
	(clear h)
	(clear c)
	(ontable g)
	(ontable a)
	(ontable e)
	(ontable h)
	(ontable c)
	(on d b)
	(on b f)
	(on f g)
)
(:goal
	(and
		(on c b)
		(on b e)
		(on e g)
		(on g f)
		(on f a)
		(on a d)
		(on d h)
	)
)
)