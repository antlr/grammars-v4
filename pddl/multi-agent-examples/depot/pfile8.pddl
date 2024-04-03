(define (problem depotprob4321) (:domain depot)
(:objects
	distributor1 - distributor
	distributor0 - distributor
	depot0 - depot
	pallet5 - pallet
	pallet4 - pallet
	pallet1 - pallet
	pallet0 - pallet
	pallet3 - pallet
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
	(clear crate2)
	(at pallet1 distributor0)
	(clear crate6)
	(at pallet2 distributor1)
	(clear crate9)
	(at pallet3 distributor1)
	(clear crate7)
	(at pallet4 distributor0)
	(clear crate0)
	(at pallet5 distributor0)
	(clear crate8)
	(at truck0 distributor0)
	(at truck1 distributor0)
	(at hoist0 depot0)
	(available depot0 hoist0)
	(at hoist1 distributor0)
	(available distributor0 hoist1)
	(at hoist2 distributor1)
	(available distributor1 hoist2)
	(at crate0 distributor0)
	(on crate0 pallet4)
	(at crate1 distributor0)
	(on crate1 pallet1)
	(at crate2 depot0)
	(on crate2 pallet0)
	(at crate3 distributor0)
	(on crate3 pallet5)
	(at crate4 distributor1)
	(on crate4 pallet3)
	(at crate5 distributor0)
	(on crate5 crate1)
	(at crate6 distributor0)
	(on crate6 crate5)
	(at crate7 distributor1)
	(on crate7 crate4)
	(at crate8 distributor0)
	(on crate8 crate3)
	(at crate9 distributor1)
	(on crate9 pallet2)
)
(:goal
	(and
		(on crate0 pallet3)
		(on crate1 crate0)
		(on crate3 crate8)
		(on crate6 pallet2)
		(on crate7 pallet1)
		(on crate8 pallet4)
		(on crate9 pallet0)
	)
)
)
