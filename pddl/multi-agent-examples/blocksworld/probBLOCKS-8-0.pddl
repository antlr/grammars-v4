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
	(clear a)
	(clear d)
	(clear b)
	(clear c)
	(ontable e)
	(ontable f)
	(ontable b)
	(ontable c)
	(on a g)
	(on g e)
	(on d h)
	(on h f)
)
(:goal
	(and
		(on d f)
		(on f e)
		(on e h)
		(on h c)
		(on c a)
		(on a g)
		(on g b)
	)
)
)