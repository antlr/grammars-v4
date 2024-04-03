(define (problem depotprob6512) (:domain depot)
(:objects
	distributor1 - distributor
	distributor0 - distributor
	depot0 - depot
	pallet1 - pallet
	pallet0 - pallet
	pallet2 - pallet
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
	(clear crate7)
	(at pallet1 distributor0)
	(clear crate2)
	(at pallet2 distributor1)
	(clear crate6)
	(at truck0 distributor1)
	(at truck1 distributor1)
	(at hoist0 depot0)
	(available depot0 hoist0)
	(at hoist1 distributor0)
	(available distributor0 hoist1)
	(at hoist2 distributor1)
	(available distributor1 hoist2)
	(at crate0 depot0)
	(on crate0 pallet0)
	(at crate1 depot0)
	(on crate1 crate0)
	(at crate2 distributor0)
	(on crate2 pallet1)
	(at crate3 distributor1)
	(on crate3 pallet2)
	(at crate4 depot0)
	(on crate4 crate1)
	(at crate5 distributor1)
	(on crate5 crate3)
	(at crate6 distributor1)
	(on crate6 crate5)
	(at crate7 depot0)
	(on crate7 crate4)
)
(:goal
	(and
		(on crate0 crate4)
		(on crate2 crate6)
		(on crate4 crate7)
		(on crate5 pallet2)
		(on crate6 pallet1)
		(on crate7 pallet0)
	)
)
)
