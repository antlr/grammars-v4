(define (problem depotprob5451) (:domain depot)
(:objects
	distributor1 - distributor
	distributor0 - distributor
	crate11 - crate
	crate10 - crate
	crate13 - crate
	crate12 - crate
	depot0 - depot
	crate14 - crate
	pallet5 - pallet
	pallet4 - pallet
	pallet1 - pallet
	pallet0 - pallet
	pallet3 - pallet
	pallet2 - pallet
	crate9 - crate
	crate8 - crate
	truck1 - truck
	truck0 - truck
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
	(clear crate2)
	(at pallet1 distributor0)
	(clear crate14)
	(at pallet2 distributor1)
	(clear crate13)
	(at pallet3 distributor1)
	(clear crate10)
	(at pallet4 distributor0)
	(clear crate12)
	(at pallet5 depot0)
	(clear crate8)
	(at truck0 distributor0)
	(at truck1 distributor0)
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
	(at crate2 depot0)
	(on crate2 crate1)
	(at crate3 distributor0)
	(on crate3 pallet1)
	(at crate4 distributor1)
	(on crate4 crate0)
	(at crate5 distributor1)
	(on crate5 pallet3)
	(at crate6 distributor0)
	(on crate6 crate3)
	(at crate7 distributor0)
	(on crate7 crate6)
	(at crate8 depot0)
	(on crate8 pallet5)
	(at crate9 distributor0)
	(on crate9 crate7)
	(at crate10 distributor1)
	(on crate10 crate5)
	(at crate11 distributor0)
	(on crate11 pallet4)
	(at crate12 distributor0)
	(on crate12 crate11)
	(at crate13 distributor1)
	(on crate13 crate4)
	(at crate14 distributor0)
	(on crate14 crate9)
)
(:goal
	(and
		(on crate0 crate5)
		(on crate1 crate2)
		(on crate2 crate10)
		(on crate3 pallet0)
		(on crate4 crate6)
		(on crate5 pallet5)
		(on crate6 pallet4)
		(on crate9 crate1)
		(on crate10 pallet2)
		(on crate11 pallet1)
		(on crate12 crate14)
		(on crate13 crate3)
		(on crate14 pallet3)
	)
)
)
