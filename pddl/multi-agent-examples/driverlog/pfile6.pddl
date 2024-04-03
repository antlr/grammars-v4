(define (problem DLOG-3-3-5) (:domain driverlog)
(:objects
	truck1 - truck
	truck3 - truck
	truck2 - truck
	package4 - package
	s1 - location
	s0 - location
	p2-0 - location
	s2 - location
	package1 - package
	package2 - package
	package3 - package
	p1-2 - location
	package5 - package

	(:private driver1
		driver1 - driver
	)

	(:private driver2
		driver2 - driver
	)

	(:private driver3
		driver3 - driver
	)
)
(:init
	(at driver1 s2)
	(at driver2 s2)
	(at driver3 s1)
	(at truck1 s0)
	(empty truck1)
	(at truck2 s1)
	(empty truck2)
	(at truck3 s1)
	(empty truck3)
	(at package1 s1)
	(at package2 s1)
	(at package3 s0)
	(at package4 s1)
	(at package5 s1)
	(path s1 p1-2)
	(path p1-2 s1)
	(path s2 p1-2)
	(path p1-2 s2)
	(path s2 p2-0)
	(path p2-0 s2)
	(path s0 p2-0)
	(path p2-0 s0)
	(link s0 s1)
	(link s1 s0)
	(link s0 s2)
	(link s2 s0)
	(link s2 s1)
	(link s1 s2)
)
(:goal
	(and
		(at truck1 s0)
		(at truck2 s0)
		(at truck3 s2)
		(at package1 s2)
		(at package2 s1)
		(at package3 s0)
		(at package4 s1)
		(at package5 s1)
	)
)
)