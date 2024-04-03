(define (problem strips-sat-x-1) (:domain satellite)
(:objects
	planet11 - direction
	planet10 - direction
	planet13 - direction
	planet15 - direction
	planet17 - direction
	planet16 - direction
	satellite2 - satellite
	satellite1 - satellite
	satellite4 - satellite
	phenomenon18 - direction
	star14 - direction
	thermograph1 - mode
	thermograph0 - mode
	phenomenon12 - direction
	thermograph2 - mode
	star19 - direction
	groundstation3 - direction
	groundstation2 - direction
	planet28 - direction
	planet29 - direction
	planet20 - direction
	planet23 - direction
	planet24 - direction
	planet25 - direction
	planet7 - direction
	phenomenon5 - direction
	image3 - mode
	planet9 - direction
	planet8 - direction
	image4 - mode
	star21 - direction
	star22 - direction
	star26 - direction
	phenomenon27 - direction
	star4 - direction
	planet6 - direction
	star1 - direction
	star0 - direction

	(:private satellite0
		instrument1 - instrument
		satellite0 - satellite
		instrument0 - instrument
	)

	(:private satellite1
		instrument2 - instrument
	)

	(:private satellite2
		instrument3 - instrument
		instrument4 - instrument
	)

	(:private satellite3
		satellite3 - satellite
		instrument5 - instrument
	)

	(:private satellite4
		instrument8 - instrument
		instrument6 - instrument
		instrument7 - instrument
	)
)
(:init
	(supports instrument0 image4)
	(calibration_target instrument0 groundstation3)
	(supports instrument1 thermograph1)
	(supports instrument1 image4)
	(calibration_target instrument1 groundstation3)
	(on_board instrument0 satellite0)
	(on_board instrument1 satellite0)
	(power_avail satellite0)
	(pointing satellite0 star19)
	(supports instrument2 thermograph0)
	(supports instrument2 image4)
	(supports instrument2 thermograph2)
	(calibration_target instrument2 groundstation3)
	(on_board instrument2 satellite1)
	(power_avail satellite1)
	(pointing satellite1 planet17)
	(supports instrument3 image4)
	(supports instrument3 image3)
	(calibration_target instrument3 star1)
	(supports instrument4 image3)
	(calibration_target instrument4 groundstation3)
	(on_board instrument3 satellite2)
	(on_board instrument4 satellite2)
	(power_avail satellite2)
	(pointing satellite2 planet7)
	(supports instrument5 thermograph1)
	(supports instrument5 image4)
	(calibration_target instrument5 groundstation3)
	(on_board instrument5 satellite3)
	(power_avail satellite3)
	(pointing satellite3 star4)
	(supports instrument6 image3)
	(supports instrument6 thermograph1)
	(supports instrument6 thermograph0)
	(calibration_target instrument6 star4)
	(supports instrument7 thermograph2)
	(supports instrument7 thermograph0)
	(calibration_target instrument7 star0)
	(supports instrument8 image3)
	(supports instrument8 thermograph2)
	(calibration_target instrument8 groundstation3)
	(on_board instrument6 satellite4)
	(on_board instrument7 satellite4)
	(on_board instrument8 satellite4)
	(power_avail satellite4)
	(pointing satellite4 phenomenon5)
)
(:goal
	(and
		(pointing satellite1 phenomenon5)
		(pointing satellite2 planet11)
		(pointing satellite4 planet11)
		(have_image phenomenon5 thermograph1)
		(have_image planet6 image4)
		(have_image planet7 image3)
		(have_image planet8 image3)
		(have_image planet9 thermograph0)
		(have_image planet10 thermograph1)
		(have_image planet11 thermograph2)
		(have_image phenomenon12 image3)
		(have_image planet13 thermograph1)
		(have_image star14 image3)
		(have_image planet15 thermograph0)
		(have_image planet16 image3)
		(have_image planet17 image4)
		(have_image phenomenon18 image3)
		(have_image star19 thermograph0)
		(have_image star21 thermograph1)
		(have_image star22 image4)
		(have_image planet23 thermograph1)
		(have_image planet24 thermograph2)
		(have_image planet25 thermograph1)
		(have_image star26 thermograph0)
		(have_image phenomenon27 thermograph1)
		(have_image planet28 thermograph2)
		(have_image planet29 thermograph0)
	)
)
)