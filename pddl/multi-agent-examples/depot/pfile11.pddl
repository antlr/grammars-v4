(define (problem depotprob8765) (:domain depot)
(:objects
	distributor1 - distributor
	distributor0 - distributor
	distributor2 - distributor
	depot0 - depot
	depot1 - depot
	depot2 - depot
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

	(:private depot1
		hoist1 - hoist
	)

	(:private depot2
		hoist2 - hoist
	)

	(:private distributor0
		hoist3 - hoist
	)

	(:private distributor1
		hoist4 - hoist
	)

	(:private distributor2
		hoist5 - hoist
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
	(clear crate1)
	(at pallet1 depot1)
	(clear crate3)
	(at pallet2 depot2)
	(clear crate9)
	(at pallet3 distributor0)
	(clear pallet3)
	(at pallet4 distributor1)
	(clear pallet4)
	(at pallet5 distributor2)
	(clear crate8)
	(at truck0 depot2)
	(at truck1 distributor0)
	(at hoist0 depot0)
	(available depot0 hoist0)
	(at hoist1 depot1)
	(available depot1 hoist1)
	(at hoist2 depot2)
	(available depot2 hoist2)
	(at hoist3 distributor0)
	(available distributor0 hoist3)
	(at hoist4 distributor1)
	(available distributor1 hoist4)
	(at hoist5 distributor2)
	(available distributor2 hoist5)
	(at crate0 depot1)
	(on crate0 pallet1)
	(at crate1 depot0)
	(on crate1 pallet0)
	(at crate2 depot2)
	(on crate2 pallet2)
	(at crate3 depot1)
	(on crate3 crate0)
	(at crate4 depot2)
	(on crate4 crate2)
	(at crate5 depot2)
	(on crate5 crate4)
	(at crate6 distributor2)
	(on crate6 pallet5)
	(at crate7 distributor2)
	(on crate7 crate6)
	(at crate8 distributor2)
	(on crate8 crate7)
	(at crate9 depot2)
	(on crate9 crate5)
)
(:goal
	(and
		(on crate0 crate7)
		(on crate1 pallet4)
		(on crate2 pallet5)
		(on crate3 crate9)
		(on crate4 pallet0)
		(on crate5 pallet2)
		(on crate6 crate5)
		(on crate7 crate1)
		(on crate8 pallet3)
		(on crate9 crate2)
	)
)
)
