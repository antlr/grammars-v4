(define (problem depotprob4398) (:domain depot)
(:objects
	distributor1 - distributor
	distributor0 - distributor
	depot0 - depot
	depot1 - depot
	pallet5 - pallet
	pallet4 - pallet
	pallet7 - pallet
	pallet6 - pallet
	pallet1 - pallet
	pallet0 - pallet
	pallet3 - pallet
	pallet2 - pallet
	truck1 - truck
	truck0 - truck
	truck3 - truck
	truck2 - truck
	crate5 - crate
	crate4 - crate
	crate1 - crate
	crate0 - crate
	crate3 - crate
	crate2 - crate

	(:private depot0
		hoist0 - hoist
	)

	(:private depot1
		hoist5 - hoist
		hoist6 - hoist
		hoist1 - hoist
	)

	(:private distributor0
		hoist2 - hoist
	)

	(:private distributor1
		hoist4 - hoist
		hoist7 - hoist
		hoist3 - hoist
	)

	(:private driver0
		driver0 - driver
	)

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
	(driving driver0 truck0)
	(driving driver1 truck1)
	(driving driver2 truck2)
	(driving driver3 truck3)
	(at pallet0 depot0)
	(clear crate5)
	(at pallet1 depot1)
	(clear crate3)
	(at pallet2 distributor0)
	(clear crate4)
	(at pallet3 distributor1)
	(clear pallet3)
	(at pallet4 depot1)
	(clear crate0)
	(at pallet5 distributor1)
	(clear pallet5)
	(at pallet6 depot1)
	(clear pallet6)
	(at pallet7 distributor0)
	(clear pallet7)
	(at truck0 depot1)
	(at truck1 depot1)
	(at truck2 depot0)
	(at truck3 distributor1)
	(at hoist0 depot0)
	(available depot0 hoist0)
	(at hoist1 depot1)
	(available depot1 hoist1)
	(at hoist2 distributor0)
	(available distributor0 hoist2)
	(at hoist3 distributor1)
	(available distributor1 hoist3)
	(at hoist4 distributor1)
	(available distributor1 hoist4)
	(at hoist5 depot1)
	(available depot1 hoist5)
	(at hoist6 depot1)
	(available depot1 hoist6)
	(at hoist7 distributor1)
	(available distributor1 hoist7)
	(at crate0 depot1)
	(on crate0 pallet4)
	(at crate1 depot1)
	(on crate1 pallet1)
	(at crate2 depot0)
	(on crate2 pallet0)
	(at crate3 depot1)
	(on crate3 crate1)
	(at crate4 distributor0)
	(on crate4 pallet2)
	(at crate5 depot0)
	(on crate5 crate2)
)
(:goal
	(and
		(on crate0 pallet3)
		(on crate2 pallet1)
		(on crate3 pallet0)
		(on crate4 crate3)
		(on crate5 pallet2)
	)
)
)
