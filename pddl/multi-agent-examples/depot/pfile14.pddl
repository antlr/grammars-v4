(define (problem depotprob7654) (:domain depot)
(:objects
	distributor1 - distributor
	distributor0 - distributor
	distributor2 - distributor
	depot0 - depot
	depot1 - depot
	depot2 - depot
	pallet5 - pallet
	pallet4 - pallet
	pallet7 - pallet
	pallet6 - pallet
	pallet1 - pallet
	pallet0 - pallet
	pallet3 - pallet
	pallet2 - pallet
	crate9 - crate
	pallet9 - pallet
	pallet8 - pallet
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
	(clear crate4)
	(at pallet1 depot1)
	(clear crate8)
	(at pallet2 depot2)
	(clear pallet2)
	(at pallet3 distributor0)
	(clear crate9)
	(at pallet4 distributor1)
	(clear crate7)
	(at pallet5 distributor2)
	(clear pallet5)
	(at pallet6 distributor2)
	(clear crate3)
	(at pallet7 depot1)
	(clear pallet7)
	(at pallet8 distributor1)
	(clear crate0)
	(at pallet9 depot0)
	(clear crate5)
	(at truck0 depot1)
	(at truck1 depot2)
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
	(at crate0 distributor1)
	(on crate0 pallet8)
	(at crate1 depot0)
	(on crate1 pallet9)
	(at crate2 distributor0)
	(on crate2 pallet3)
	(at crate3 distributor2)
	(on crate3 pallet6)
	(at crate4 depot0)
	(on crate4 pallet0)
	(at crate5 depot0)
	(on crate5 crate1)
	(at crate6 distributor1)
	(on crate6 pallet4)
	(at crate7 distributor1)
	(on crate7 crate6)
	(at crate8 depot1)
	(on crate8 pallet1)
	(at crate9 distributor0)
	(on crate9 crate2)
)
(:goal
	(and
		(on crate1 pallet8)
		(on crate2 pallet3)
		(on crate4 pallet0)
		(on crate5 pallet5)
		(on crate6 pallet1)
		(on crate7 crate6)
		(on crate9 crate7)
	)
)
)
