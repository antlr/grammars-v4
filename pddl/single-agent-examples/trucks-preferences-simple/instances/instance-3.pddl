(define (problem truck-3)
(:domain Trucks-SimplePreferences)
(:objects
	truck1 - truck
	package1 - package
	package2 - package
	package3 - package
	package4 - package
	package5 - package
	l1 - location
	l2 - location
	l3 - location
	t0 - time
	t1 - time
	t2 - time
	t3 - time
	t4 - time
	t5 - time
	t6 - time
	t7 - time
	t8 - time
	t9 - time
	t10 - time
	a1 - truckarea
	a2 - truckarea)

(:init
	(at truck1 l2)
	(free a1 truck1)
	(free a2 truck1)
	(closer a1 a2)
	(at package1 l1)
	(at package2 l1)
	(at package3 l1)
	(at package4 l1)
	(at package5 l2)
	(connected l1 l2)
	(connected l1 l3)
	(connected l2 l1)
	(connected l2 l3)
	(connected l3 l1)
	(connected l3 l2)
	(time-now t0)
	(le t1 t1)
	(le t1 t2)
	(le t1 t3)
	(le t1 t4)
	(le t1 t5)
	(le t1 t6)
	(le t1 t7)
	(le t1 t8)
	(le t1 t9)
	(le t1 t10)
	(le t2 t2)
	(le t2 t3)
	(le t2 t4)
	(le t2 t5)
	(le t2 t6)
	(le t2 t7)
	(le t2 t8)
	(le t2 t9)
	(le t2 t10)
	(le t3 t3)
	(le t3 t4)
	(le t3 t5)
	(le t3 t6)
	(le t3 t7)
	(le t3 t8)
	(le t3 t9)
	(le t3 t10)
	(le t4 t4)
	(le t4 t5)
	(le t4 t6)
	(le t4 t7)
	(le t4 t8)
	(le t4 t9)
	(le t4 t10)
	(le t5 t5)
	(le t5 t6)
	(le t5 t7)
	(le t5 t8)
	(le t5 t9)
	(le t5 t10)
	(le t6 t6)
	(le t6 t7)
	(le t6 t8)
	(le t6 t9)
	(le t6 t10)
	(le t7 t7)
	(le t7 t8)
	(le t7 t9)
	(le t7 t10)
	(le t8 t8)
	(le t8 t9)
	(le t8 t10)
	(le t9 t9)
	(le t9 t10)
	(le t10 t10)
	(next t0 t1)
	(next t1 t2)
	(next t2 t3)
	(next t3 t4)
	(next t4 t5)
	(next t5 t6)
	(next t6 t7)
	(next t7 t8)
	(next t8 t9)
	(next t9 t10))

(:goal (and 

	(at-destination package1 l2)

	(at-destination package2 l2)
	(preference p1A (exists (?t - time)
		 (and (delivered package2 l2 ?t) (le ?t t2))))
	(preference p2A (exists (?t - time)
		 (and (delivered package2 l2 ?t) (le ?t t3))))
	(preference p3A (exists (?t - time)
		 (and (delivered package2 l2 ?t) (le ?t t4))))
	(preference p4A (exists (?t - time)
		 (and (delivered package2 l2 ?t) (le ?t t5))))
	(preference p5A (exists (?t - time)
		 (and (delivered package2 l2 ?t) (le ?t t6))))
	(preference p6A (exists (?t - time)
		 (and (delivered package2 l2 ?t) (le ?t t7))))
	(preference p7A (exists (?t - time)
		 (and (delivered package2 l2 ?t) (le ?t t8))))
	(preference p8A (exists (?t - time)
		 (and (delivered package2 l2 ?t) (le ?t t9))))
	(preference p9A (exists (?t - time)
		 (and (delivered package2 l2 ?t) (le ?t t10))))

	(at-destination package3 l3)


	(at-destination package4 l3)


	(at-destination package5 l3)
))

(:metric minimize (+ (* 1 (is-violated p1A))
		     (* 2 (is-violated p2A))
		     (* 3 (is-violated p3A))
		     (* 4 (is-violated p4A))
		     (* 5 (is-violated p5A))
		     (* 6 (is-violated p6A))
		     (* 7 (is-violated p7A))
		     (* 8 (is-violated p8A))
		     (* 9 (is-violated p9A))))

)
