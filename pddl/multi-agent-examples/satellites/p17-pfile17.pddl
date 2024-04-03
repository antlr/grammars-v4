(define (problem strips-sat-x-1) (:domain satellite)
(:objects
	planet11 - direction
	planet13 - direction
	planet12 - direction
	planet16 - direction
	star18 - direction
	satellite8 - satellite
	satellite1 - satellite
	satellite4 - satellite
	phenomenon19 - direction
	star10 - direction
	thermograph1 - mode
	phenomenon14 - direction
	phenomenon15 - direction
	phenomenon17 - direction
	image4 - mode
	groundstation2 - direction
	groundstation0 - direction
	star21 - direction
	phenomenon23 - direction
	planet5 - direction
	planet7 - direction
	planet6 - direction
	image0 - mode
	phenomenon9 - direction
	phenomenon20 - direction
	star22 - direction
	groundstation4 - direction
	phenomenon24 - direction
	infrared2 - mode
	infrared3 - mode
	star1 - direction
	star3 - direction
	star8 - direction

	(:private satellite0
		instrument0 - instrument
		satellite0 - satellite
	)

	(:private satellite1
		instrument2 - instrument
		instrument1 - instrument
	)

	(:private satellite2
		satellite2 - satellite
		instrument5 - instrument
		instrument4 - instrument
		instrument3 - instrument
	)

	(:private satellite3
		satellite3 - satellite
		instrument8 - instrument
		instrument6 - instrument
		instrument7 - instrument
	)

	(:private satellite4
		instrument9 - instrument
		instrument10 - instrument
	)

	(:private satellite5
		instrument11 - instrument
		satellite5 - satellite
	)

	(:private satellite6
		instrument14 - instrument
		satellite6 - satellite
		instrument12 - instrument
		instrument13 - instrument
	)

	(:private satellite7
		instrument15 - instrument
		satellite7 - satellite
	)

	(:private satellite8
		instrument16 - instrument
		instrument17 - instrument
	)

	(:private satellite9
		satellite9 - satellite
		instrument18 - instrument
		instrument19 - instrument
	)

	(:private satellite10
		satellite10 - satellite
		instrument21 - instrument
		instrument20 - instrument
		instrument22 - instrument
	)

	(:private satellite11
		satellite11 - satellite
		instrument23 - instrument
	)
)
(:init
	(supports instrument0 infrared3)
	(calibration_target instrument0 groundstation0)
	(on_board instrument0 satellite0)
	(power_avail satellite0)
	(pointing satellite0 planet5)
	(supports instrument1 image0)
	(supports instrument1 infrared2)
	(calibration_target instrument1 star3)
	(supports instrument2 thermograph1)
	(supports instrument2 image0)
	(calibration_target instrument2 groundstation0)
	(on_board instrument1 satellite1)
	(on_board instrument2 satellite1)
	(power_avail satellite1)
	(pointing satellite1 planet5)
	(supports instrument3 infrared3)
	(supports instrument3 infrared2)
	(calibration_target instrument3 groundstation4)
	(supports instrument4 infrared3)
	(supports instrument4 infrared2)
	(supports instrument4 thermograph1)
	(calibration_target instrument4 groundstation2)
	(supports instrument5 thermograph1)
	(calibration_target instrument5 groundstation4)
	(on_board instrument3 satellite2)
	(on_board instrument4 satellite2)
	(on_board instrument5 satellite2)
	(power_avail satellite2)
	(pointing satellite2 star21)
	(supports instrument6 image0)
	(supports instrument6 infrared2)
	(calibration_target instrument6 groundstation2)
	(supports instrument7 image0)
	(supports instrument7 infrared3)
	(calibration_target instrument7 groundstation0)
	(supports instrument8 infrared2)
	(supports instrument8 image4)
	(supports instrument8 image0)
	(calibration_target instrument8 groundstation4)
	(on_board instrument6 satellite3)
	(on_board instrument7 satellite3)
	(on_board instrument8 satellite3)
	(power_avail satellite3)
	(pointing satellite3 star21)
	(supports instrument9 infrared3)
	(calibration_target instrument9 star1)
	(supports instrument10 image4)
	(supports instrument10 image0)
	(calibration_target instrument10 star3)
	(on_board instrument9 satellite4)
	(on_board instrument10 satellite4)
	(power_avail satellite4)
	(pointing satellite4 star22)
	(supports instrument11 infrared2)
	(calibration_target instrument11 star1)
	(on_board instrument11 satellite5)
	(power_avail satellite5)
	(pointing satellite5 groundstation2)
	(supports instrument12 image4)
	(calibration_target instrument12 groundstation0)
	(supports instrument13 image4)
	(calibration_target instrument13 star1)
	(supports instrument14 thermograph1)
	(supports instrument14 infrared2)
	(calibration_target instrument14 groundstation2)
	(on_board instrument12 satellite6)
	(on_board instrument13 satellite6)
	(on_board instrument14 satellite6)
	(power_avail satellite6)
	(pointing satellite6 phenomenon20)
	(supports instrument15 image0)
	(supports instrument15 thermograph1)
	(calibration_target instrument15 groundstation4)
	(on_board instrument15 satellite7)
	(power_avail satellite7)
	(pointing satellite7 planet12)
	(supports instrument16 image0)
	(supports instrument16 infrared2)
	(calibration_target instrument16 star1)
	(supports instrument17 infrared3)
	(calibration_target instrument17 groundstation0)
	(on_board instrument16 satellite8)
	(on_board instrument17 satellite8)
	(power_avail satellite8)
	(pointing satellite8 phenomenon23)
	(supports instrument18 thermograph1)
	(supports instrument18 infrared2)
	(supports instrument18 image4)
	(calibration_target instrument18 star1)
	(supports instrument19 infrared3)
	(calibration_target instrument19 groundstation4)
	(on_board instrument18 satellite9)
	(on_board instrument19 satellite9)
	(power_avail satellite9)
	(pointing satellite9 phenomenon20)
	(supports instrument20 infrared2)
	(calibration_target instrument20 star1)
	(supports instrument21 thermograph1)
	(supports instrument21 image0)
	(calibration_target instrument21 star1)
	(supports instrument22 thermograph1)
	(calibration_target instrument22 groundstation0)
	(on_board instrument20 satellite10)
	(on_board instrument21 satellite10)
	(on_board instrument22 satellite10)
	(power_avail satellite10)
	(pointing satellite10 star22)
	(supports instrument23 infrared2)
	(supports instrument23 image4)
	(supports instrument23 thermograph1)
	(calibration_target instrument23 star3)
	(on_board instrument23 satellite11)
	(power_avail satellite11)
	(pointing satellite11 star8)
)
(:goal
	(and
		(pointing satellite1 star22)
		(pointing satellite4 phenomenon20)
		(pointing satellite8 planet16)
		(have_image planet5 image0)
		(have_image planet6 image4)
		(have_image planet7 image4)
		(have_image phenomenon9 image4)
		(have_image star10 thermograph1)
		(have_image planet11 image4)
		(have_image planet12 thermograph1)
		(have_image planet13 infrared3)
		(have_image phenomenon14 infrared2)
		(have_image phenomenon15 infrared2)
		(have_image planet16 infrared2)
		(have_image phenomenon17 thermograph1)
		(have_image star18 image4)
		(have_image star21 thermograph1)
		(have_image star22 image4)
		(have_image phenomenon23 infrared3)
		(have_image phenomenon24 infrared3)
	)
)
)