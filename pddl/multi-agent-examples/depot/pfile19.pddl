(define (problem depotprob6178) (:domain depot)
(:objects
	distributor1 - distributor
	distributor0 - distributor
	distributor3 - distributor
	distributor2 - distributor
	depot0 - depot
	depot1 - depot
	depot2 - depot
	depot3 - depot
	pallet5 - pallet
	pallet4 - pallet
	pallet7 - pallet
	pallet6 - pallet
	pallet1 - pallet
	pallet0 - pallet
	pallet3 - pallet
	pallet2 - pallet
	pallet9 - pallet
	pallet8 - pallet
	truck1 - truck
	truck0 - truck
	truck3 - truck
	truck2 - truck
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

	(:private depot3
		hoist3 - hoist
	)

	(:private distributor0
		hoist4 - hoist
	)

	(:private distributor1
		hoist5 - hoist
	)

	(:private distributor2
		hoist6 - hoist
	)

	(:private distributor3
		hoist7 - hoist
	)

	(:private driver0
		driver0 - driver
	)

	(:private driver1
		driver1 - driver
	)

	
)
(:init
	(driving driver0 truck0)
	(driving driver1 truck1)
	(driving driver0 truck2)
	(driving driver1 truck3)
	(at pallet0 depot0)
	(clear crate6)
	(at pallet1 depot1)
	(clear crate1)
	(at pallet2 depot2)
	(clear pallet2)
	(at pallet3 depot3)
	(clear crate7)
	(at pallet4 distributor0)
	(clear crate2)
	(at pallet5 distributor1)
	(clear crate5)
	(at pallet6 distributor2)
	(clear pallet6)
	(at pallet7 distributor3)
	(clear pallet7)
	(at pallet8 distributor2)
	(clear crate4)
	(at pallet9 depot3)
	(clear crate0)
	(at truck0 depot0)
	(at truck1 distributor0)
	(at truck2 depot2)
	(at truck3 distributor3)
	(at hoist0 depot0)
	(available depot0 hoist0)
	(at hoist1 depot1)
	(available depot1 hoist1)
	(at hoist2 depot2)
	(available depot2 hoist2)
	(at hoist3 depot3)
	(available depot3 hoist3)
	(at hoist4 distributor0)
	(available distributor0 hoist4)
	(at hoist5 distributor1)
	(available distributor1 hoist5)
	(at hoist6 distributor2)
	(available distributor2 hoist6)
	(at hoist7 distributor3)
	(available distributor3 hoist7)
	(at crate0 depot3)
	(on crate0 pallet9)
	(at crate1 depot1)
	(on crate1 pallet1)
	(at crate2 distributor0)
	(on crate2 pallet4)
	(at crate3 distributor1)
	(on crate3 pallet5)
	(at crate4 distributor2)
	(on crate4 pallet8)
	(at crate5 distributor1)
	(on crate5 crate3)
	(at crate6 depot0)
	(on crate6 pallet0)
	(at crate7 depot3)
	(on crate7 pallet3)
)
(:goal
	(and
		(on crate0 pallet6)
		(on crate1 pallet8)
		(on crate3 crate1)
		(on crate4 pallet5)
		(on crate5 crate7)
		(on crate6 pallet4)
		(on crate7 crate4)
	)
)
)
