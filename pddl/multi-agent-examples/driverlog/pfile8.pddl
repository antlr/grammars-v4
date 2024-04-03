(define (problem DLOG-3-3-7) (:domain driverlog)
(:objects
	p2-1 - location
	p2-0 - location
	package1 - package
	package2 - package
	package3 - package
	package4 - package
	package5 - package
	package6 - package
	package7 - package
	truck1 - truck
	truck3 - truck
	truck2 - truck
	s2 - location
	s1 - location
	s0 - location
	p1-0 - location

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
	(at driver2 s0)
	(at driver3 s1)
	(at truck1 s2)
	(empty truck1)
	(at truck2 s2)
	(empty truck2)
	(at truck3 s2)
	(empty truck3)
	(at package1 s0)
	(at package2 s1)
	(at package3 s0)
	(at package4 s0)
	(at package5 s1)
	(at package6 s2)
	(at package7 s2)
	(path s1 p1-0)
	(path p1-0 s1)
	(path s0 p1-0)
	(path p1-0 s0)
	(path s2 p2-0)
	(path p2-0 s2)
	(path s0 p2-0)
	(path p2-0 s0)
	(path s2 p2-1)
	(path p2-1 s2)
	(path s1 p2-1)
	(path p2-1 s1)
	(link s0 s1)
	(link s1 s0)
	(link s0 s2)
	(link s2 s0)
	(link s1 s2)
	(link s2 s1)
)
(:goal
	(and
		(at truck2 s1)
		(at truck3 s0)
		(at package1 s2)
		(at package2 s0)
		(at package3 s1)
		(at package4 s2)
		(at package5 s1)
		(at package6 s2)
		(at package7 s1)
	)
)
)