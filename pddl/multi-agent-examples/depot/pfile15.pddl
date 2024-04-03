(define (problem depotprob4534) (:domain depot)
(:objects
	crate11 - crate
	crate10 - crate
	crate13 - crate
	crate12 - crate
	depot0 - depot
	crate14 - crate
	depot2 - depot
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
	distributor1 - distributor
	distributor0 - distributor
	distributor2 - distributor
	depot1 - depot
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
	(clear pallet0)
	(at pallet1 depot1)
	(clear crate7)
	(at pallet2 depot2)
	(clear pallet2)
	(at pallet3 distributor0)
	(clear crate8)
	(at pallet4 distributor1)
	(clear crate12)
	(at pallet5 distributor2)
	(clear crate11)
	(at pallet6 depot1)
	(clear crate4)
	(at pallet7 distributor0)
	(clear crate9)
	(at pallet8 depot2)
	(clear crate13)
	(at pallet9 distributor0)
	(clear crate14)
	(at truck0 distributor1)
	(at truck1 distributor2)
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
	(at crate0 distributor2)
	(on crate0 pallet5)
	(at crate1 distributor1)
	(on crate1 pallet4)
	(at crate2 depot2)
	(on crate2 pallet8)
	(at crate3 depot2)
	(on crate3 crate2)
	(at crate4 depot1)
	(on crate4 pallet6)
	(at crate5 distributor2)
	(on crate5 crate0)
	(at crate6 depot1)
	(on crate6 pallet1)
	(at crate7 depot1)
	(on crate7 crate6)
	(at crate8 distributor0)
	(on crate8 pallet3)
	(at crate9 distributor0)
	(on crate9 pallet7)
	(at crate10 distributor1)
	(on crate10 crate1)
	(at crate11 distributor2)
	(on crate11 crate5)
	(at crate12 distributor1)
	(on crate12 crate10)
	(at crate13 depot2)
	(on crate13 crate3)
	(at crate14 distributor0)
	(on crate14 pallet9)
)
(:goal
	(and
		(on crate0 crate8)
		(on crate1 crate10)
		(on crate2 pallet0)
		(on crate3 pallet1)
		(on crate4 crate7)
		(on crate5 pallet5)
		(on crate6 pallet6)
		(on crate7 pallet4)
		(on crate8 pallet7)
		(on crate9 crate4)
		(on crate10 crate11)
		(on crate11 crate9)
		(on crate12 crate5)
		(on crate13 pallet8)
		(on crate14 pallet9)
	)
)
)
