(define (problem DLOG-2-2-2) (:domain driverlog)
(:objects
	truck1 - truck
	truck2 - truck
	s2 - location
	s1 - location
	s0 - location
	package1 - package
	package2 - package
	p1-2 - location
	p1-0 - location

	(:private driver1
		driver1 - driver
	)

	(:private driver2
		driver2 - driver
	)
)
(:init
	(at driver1 s2)
	(at driver2 s2)
	(at truck1 s0)
	(empty truck1)
	(at truck2 s0)
	(empty truck2)
	(at package1 s0)
	(at package2 s0)
	(path s1 p1-0)
	(path p1-0 s1)
	(path s0 p1-0)
	(path p1-0 s0)
	(path s1 p1-2)
	(path p1-2 s1)
	(path s2 p1-2)
	(path p1-2 s2)
	(link s0 s1)
	(link s1 s0)
	(link s0 s2)
	(link s2 s0)
	(link s2 s1)
	(link s1 s2)
)
(:goal
	(and
		(at truck1 s1)
		(at package1 s0)
		(at package2 s0)
	)
)
)