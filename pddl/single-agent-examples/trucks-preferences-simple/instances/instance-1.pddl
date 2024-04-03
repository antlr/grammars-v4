(define (problem truck-1)
(:domain Trucks-SimplePreferences)
(:objects
	truck1 - truck
	package1 - package
	package2 - package
	package3 - package
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
	a1 - truckarea
	a2 - truckarea)

(:init
	(at truck1 l3)
	(free a1 truck1)
	(free a2 truck1)
	(closer a1 a2)
	(at package1 l2)
	(at package2 l2)
	(at package3 l2)
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
	(le t2 t2)
	(le t2 t3)
	(le t2 t4)
	(le t2 t5)
	(le t2 t6)
	(le t3 t3)
	(le t3 t4)
	(le t3 t5)
	(le t3 t6)
	(le t4 t4)
	(le t4 t5)
	(le t4 t6)
	(le t5 t5)
	(le t5 t6)
	(le t6 t6)
	(next t0 t1)
	(next t1 t2)
	(next t2 t3)
	(next t3 t4)
	(next t4 t5)
	(next t5 t6))

(:goal (and 
	(at-destination package1 l3)
	(preference p1A (exists (?t - time)
		 (and (delivered package1 l3 ?t) (le ?t t2))))
	(preference p2A (exists (?t - time)
		 (and (delivered package1 l3 ?t) (le ?t t3))))
	(preference p3A (exists (?t - time)
		 (and (delivered package1 l3 ?t) (le ?t t4))))
	(preference p4A (exists (?t - time)
		 (and (delivered package1 l3 ?t) (le ?t t5))))
	(preference p5A (exists (?t - time)
		 (and (delivered package1 l3 ?t) (le ?t t6))))

	(at-destination package2 l1)

	(at-destination package3 l1)
	(preference p1B (exists (?t - time)
		 (and (delivered package3 l1 ?t) (le ?t t4))))
	(preference p2B (exists (?t - time)
		 (and (delivered package3 l1 ?t) (le ?t t5))))
	(preference p3B (exists (?t - time)
		 (and (delivered package3 l1 ?t) (le ?t t6))))))

(:metric minimize (+ (* 1 (is-violated p1A))
		     (* 1 (is-violated p1B))
		     (* 2 (is-violated p2A))
		     (* 2 (is-violated p2B))
		     (* 3 (is-violated p3A))
		     (* 3 (is-violated p3B))
		     (* 4 (is-violated p4A))
		     (* 5 (is-violated p5A))))

)
