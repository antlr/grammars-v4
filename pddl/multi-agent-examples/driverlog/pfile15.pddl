(define (problem DLOG-4-4-8) (:domain driverlog)
(:objects
	p10-1 - location
	p10-0 - location
	package8 - package
	package1 - package
	package2 - package
	package3 - package
	package4 - package
	package5 - package
	package6 - package
	package7 - package
	p6-9 - location
	truck4 - truck
	s9 - location
	s8 - location
	truck1 - truck
	truck3 - truck
	truck2 - truck
	s3 - location
	s2 - location
	s1 - location
	s0 - location
	s7 - location
	s6 - location
	s5 - location
	s4 - location
	p6-11 - location
	p9-4 - location
	p0-11 - location
	s11 - location
	p7-2 - location
	p8-3 - location
	p8-1 - location
	s10 - location
	p8-7 - location
	p8-4 - location
	p0-2 - location
	p2-1 - location
	p0-6 - location
	p0-5 - location
	p1-5 - location
	p2-8 - location
	p11-2 - location
	p2-4 - location
	p9-11 - location
	p4-7 - location
	p5-9 - location

	(:private driver1
		driver1 - driver
	)

	(:private driver2
		driver2 - driver
	)

	(:private driver3
		driver3 - driver
	)

	(:private driver4
		driver4 - driver
	)
)
(:init
	(at driver1 s8)
	(at driver2 s5)
	(at driver3 s5)
	(at driver4 s10)
	(at truck1 s3)
	(empty truck1)
	(at truck2 s9)
	(empty truck2)
	(at truck3 s3)
	(empty truck3)
	(at truck4 s6)
	(empty truck4)
	(at package1 s3)
	(at package2 s2)
	(at package3 s8)
	(at package4 s11)
	(at package5 s1)
	(at package6 s8)
	(at package7 s9)
	(at package8 s10)
	(path s0 p0-2)
	(path p0-2 s0)
	(path s2 p0-2)
	(path p0-2 s2)
	(path s0 p0-5)
	(path p0-5 s0)
	(path s5 p0-5)
	(path p0-5 s5)
	(path s0 p0-6)
	(path p0-6 s0)
	(path s6 p0-6)
	(path p0-6 s6)
	(path s0 p0-11)
	(path p0-11 s0)
	(path s11 p0-11)
	(path p0-11 s11)
	(path s1 p1-5)
	(path p1-5 s1)
	(path s5 p1-5)
	(path p1-5 s5)
	(path s2 p2-1)
	(path p2-1 s2)
	(path s1 p2-1)
	(path p2-1 s1)
	(path s2 p2-4)
	(path p2-4 s2)
	(path s4 p2-4)
	(path p2-4 s4)
	(path s2 p2-8)
	(path p2-8 s2)
	(path s8 p2-8)
	(path p2-8 s8)
	(path s4 p4-7)
	(path p4-7 s4)
	(path s7 p4-7)
	(path p4-7 s7)
	(path s5 p5-9)
	(path p5-9 s5)
	(path s9 p5-9)
	(path p5-9 s9)
	(path s6 p6-9)
	(path p6-9 s6)
	(path s9 p6-9)
	(path p6-9 s9)
	(path s6 p6-11)
	(path p6-11 s6)
	(path s11 p6-11)
	(path p6-11 s11)
	(path s7 p7-2)
	(path p7-2 s7)
	(path s2 p7-2)
	(path p7-2 s2)
	(path s8 p8-1)
	(path p8-1 s8)
	(path s1 p8-1)
	(path p8-1 s1)
	(path s8 p8-3)
	(path p8-3 s8)
	(path s3 p8-3)
	(path p8-3 s3)
	(path s8 p8-4)
	(path p8-4 s8)
	(path s4 p8-4)
	(path p8-4 s4)
	(path s8 p8-7)
	(path p8-7 s8)
	(path s7 p8-7)
	(path p8-7 s7)
	(path s9 p9-4)
	(path p9-4 s9)
	(path s4 p9-4)
	(path p9-4 s4)
	(path s9 p9-11)
	(path p9-11 s9)
	(path s11 p9-11)
	(path p9-11 s11)
	(path s10 p10-0)
	(path p10-0 s10)
	(path s0 p10-0)
	(path p10-0 s0)
	(path s10 p10-1)
	(path p10-1 s10)
	(path s1 p10-1)
	(path p10-1 s1)
	(path s11 p11-2)
	(path p11-2 s11)
	(path s2 p11-2)
	(path p11-2 s2)
	(link s0 s5)
	(link s5 s0)
	(link s0 s8)
	(link s8 s0)
	(link s1 s0)
	(link s0 s1)
	(link s1 s9)
	(link s9 s1)
	(link s1 s10)
	(link s10 s1)
	(link s2 s1)
	(link s1 s2)
	(link s2 s3)
	(link s3 s2)
	(link s2 s5)
	(link s5 s2)
	(link s2 s8)
	(link s8 s2)
	(link s2 s9)
	(link s9 s2)
	(link s3 s0)
	(link s0 s3)
	(link s3 s8)
	(link s8 s3)
	(link s3 s10)
	(link s10 s3)
	(link s5 s3)
	(link s3 s5)
	(link s5 s4)
	(link s4 s5)
	(link s5 s8)
	(link s8 s5)
	(link s5 s9)
	(link s9 s5)
	(link s6 s1)
	(link s1 s6)
	(link s6 s3)
	(link s3 s6)
	(link s6 s4)
	(link s4 s6)
	(link s6 s7)
	(link s7 s6)
	(link s6 s9)
	(link s9 s6)
	(link s7 s5)
	(link s5 s7)
	(link s7 s11)
	(link s11 s7)
	(link s8 s6)
	(link s6 s8)
	(link s9 s0)
	(link s0 s9)
	(link s9 s3)
	(link s3 s9)
	(link s9 s4)
	(link s4 s9)
	(link s9 s7)
	(link s7 s9)
	(link s9 s8)
	(link s8 s9)
	(link s10 s2)
	(link s2 s10)
	(link s10 s6)
	(link s6 s10)
	(link s11 s0)
	(link s0 s11)
	(link s11 s3)
	(link s3 s11)
)
(:goal
	(and
		(at truck3 s8)
		(at package1 s2)
		(at package2 s5)
		(at package3 s1)
		(at package4 s7)
		(at package5 s0)
		(at package6 s11)
		(at package7 s2)
		(at package8 s0)
	)
)
)