(define (problem depotprob5656) (:domain depot)
(:objects
	distributor1 - distributor
	distributor0 - distributor
	crate11 - crate
	crate10 - crate
	crate13 - crate
	crate12 - crate
	depot0 - depot
	crate14 - crate
	pallet1 - pallet
	pallet0 - pallet
	pallet2 - pallet
	truck1 - truck
	truck0 - truck
	crate9 - crate
	crate8 - crate
	crate5 - crate
	crate4 - crate
	crate7 - crate
	crate6 - crate
	crate1 - crate
	crate0 - crate
	crate3 - crate
	crate2 - crate

	(:private depot0
		hoist0 - hoist
	)

	(:private distributor0
		hoist1 - hoist
	)

	(:private distributor1
		hoist2 - hoist
	)

	(:private driver1
		driver1 - driver
	)

	(:private driver0
		driver0 - driver
	)
)
(:init
	(driving driver0 truck0)
	(driving driver1 truck1)
	(at pallet0 depot0)
	(clear crate11)
	(at pallet1 distributor0)
	(clear crate14)
	(at pallet2 distributor1)
	(clear crate10)
	(at truck0 distributor1)
	(at truck1 depot0)
	(at hoist0 depot0)
	(available depot0 hoist0)
	(at hoist1 distributor0)
	(available distributor0 hoist1)
	(at hoist2 distributor1)
	(available distributor1 hoist2)
	(at crate0 distributor1)
	(on crate0 pallet2)
	(at crate1 depot0)
	(on crate1 pallet0)
	(at crate2 distributor1)
	(on crate2 crate0)
	(at crate3 distributor0)
	(on crate3 pallet1)
	(at crate4 distributor0)
	(on crate4 crate3)
	(at crate5 distributor1)
	(on crate5 crate2)
	(at crate6 depot0)
	(on crate6 crate1)
	(at crate7 distributor0)
	(on crate7 crate4)
	(at crate8 distributor0)
	(on crate8 crate7)
	(at crate9 distributor0)
	(on crate9 crate8)
	(at crate10 distributor1)
	(on crate10 crate5)
	(at crate11 depot0)
	(on crate11 crate6)
	(at crate12 distributor0)
	(on crate12 crate9)
	(at crate13 distributor0)
	(on crate13 crate12)
	(at crate14 distributor0)
	(on crate14 crate13)
)
(:goal
	(and
		(on crate0 crate8)
		(on crate1 crate9)
		(on crate2 crate1)
		(on crate3 crate12)
		(on crate4 crate11)
		(on crate5 crate0)
		(on crate8 pallet0)
		(on crate9 pallet1)
		(on crate10 crate4)
		(on crate11 crate5)
		(on crate12 pallet2)
	)
)
)
