
	declare
		seldsk	entry	(fixed(7)) returns(ptr),
		settrk	entry	(fixed(15)),
		setsec	entry	(fixed(15)),
		rdsec	entry	returns(fixed(7)),
		wrsec	entry	(fixed(7)) returns(fixed(7)),
		sectrn	entry	(fixed(15), ptr) returns(fixed(15)),
		bstdma	entry	(ptr);
