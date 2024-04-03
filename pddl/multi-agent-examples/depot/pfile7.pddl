(define (problem depotprob1234) (:domain depot)
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
	crate5 - crate
	crate4 - crate
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
	(clear crate5)
	(at pallet1 distributor0)
	(clear pallet1)
	(at pallet2 distributor1)
	(clear crate3)
	(at pallet3 distributor0)
	(clear pallet3)
	(at pallet4 distributor0)
	(clear crate4)
	(at pallet5 distributor1)
	(clear crate1)
	(at truck0 distributor1)
	(at truck1 depot0)
	(at hoist0 depot0)
	(available depot0 hoist0)
	(at hoist1 distributor0)
	(available distributor0 hoist1)
	(at hoist2 distributor1)
	(available distributor1 hoist2)
	(at crate0 distributor0)
	(on crate0 pallet4)
	(at crate1 distributor1)
	(on crate1 pallet5)
	(at crate2 distributor1)
	(on crate2 pallet2)
	(at crate3 distributor1)
	(on crate3 crate2)
	(at crate4 distributor0)
	(on crate4 crate0)
	(at crate5 depot0)
	(on crate5 pallet0)
)
(:goal
	(and
		(on crate0 pallet3)
		(on crate1 crate4)
		(on crate3 pallet1)
		(on crate4 pallet5)
		(on crate5 crate1)
	)
)
)
