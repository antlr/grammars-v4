(define (problem strips-sat-x-1) (:domain satellite)
(:objects
	planet15 - direction
	planet17 - direction
	satellite0 - satellite
	star14 - direction
	star16 - direction
	star10 - direction
	star12 - direction
	thermograph1 - mode
	phenomenon11 - direction
	phenomenon13 - direction
	star19 - direction
	star18 - direction
	star6 - direction
	phenomenon8 - direction
	groundstation4 - direction
	groundstation0 - direction
	planet21 - direction
	planet22 - direction
	phenomenon7 - direction
	phenomenon5 - direction
	planet9 - direction
	image5 - mode
	image4 - mode
	phenomenon20 - direction
	star23 - direction
	phenomenon24 - direction
	infrared7 - mode
	infrared2 - mode
	infrared3 - mode
	spectrograph0 - mode
	star1 - direction
	spectrograph6 - mode
	star3 - direction
	star2 - direction

	(:private satellite0
		instrument2 - instrument
		instrument0 - instrument
		instrument1 - instrument
	)

	(:private satellite1
		instrument3 - instrument
		instrument6 - instrument
		instrument7 - instrument
		instrument4 - instrument
		instrument5 - instrument
		satellite1 - satellite
	)

	(:private satellite2
		instrument8 - instrument
		instrument9 - instrument
		satellite2 - satellite
		instrument10 - instrument
		instrument11 - instrument
		instrument12 - instrument
		instrument13 - instrument
	)

	(:private satellite3
		satellite3 - satellite
		instrument21 - instrument
		instrument20 - instrument
		instrument15 - instrument
		instrument14 - instrument
		instrument18 - instrument
		instrument16 - instrument
		instrument17 - instrument
		instrument19 - instrument
	)

	(:private satellite4
		instrument24 - instrument
		instrument22 - instrument
		instrument23 - instrument
		satellite4 - satellite
	)
)
(:init
	(supports instrument0 infrared2)
	(supports instrument0 image4)
	(calibration_target instrument0 groundstation0)
	(supports instrument1 spectrograph0)
	(calibration_target instrument1 star3)
	(supports instrument2 infrared3)
	(supports instrument2 thermograph1)
	(supports instrument2 spectrograph0)
	(calibration_target instrument2 star1)
	(on_board instrument0 satellite0)
	(on_board instrument1 satellite0)
	(on_board instrument2 satellite0)
	(power_avail satellite0)
	(pointing satellite0 planet21)
	(supports instrument3 thermograph1)
	(supports instrument3 image5)
	(calibration_target instrument3 star1)
	(supports instrument4 spectrograph0)
	(calibration_target instrument4 star3)
	(supports instrument5 thermograph1)
	(supports instrument5 spectrograph0)
	(supports instrument5 spectrograph6)
	(calibration_target instrument5 groundstation4)
	(supports instrument6 image5)
	(supports instrument6 infrared7)
	(calibration_target instrument6 star3)
	(supports instrument7 spectrograph6)
	(supports instrument7 thermograph1)
	(supports instrument7 spectrograph0)
	(calibration_target instrument7 star2)
	(on_board instrument3 satellite1)
	(on_board instrument4 satellite1)
	(on_board instrument5 satellite1)
	(on_board instrument6 satellite1)
	(on_board instrument7 satellite1)
	(power_avail satellite1)
	(pointing satellite1 planet21)
	(supports instrument8 infrared3)
	(supports instrument8 infrared7)
	(calibration_target instrument8 star1)
	(supports instrument9 spectrograph0)
	(calibration_target instrument9 star3)
	(supports instrument10 image4)
	(supports instrument10 infrared7)
	(supports instrument10 image5)
	(calibration_target instrument10 groundstation4)
	(supports instrument11 infrared2)
	(calibration_target instrument11 star2)
	(supports instrument12 thermograph1)
	(calibration_target instrument12 star3)
	(supports instrument13 infrared3)
	(calibration_target instrument13 star2)
	(on_board instrument8 satellite2)
	(on_board instrument9 satellite2)
	(on_board instrument10 satellite2)
	(on_board instrument11 satellite2)
	(on_board instrument12 satellite2)
	(on_board instrument13 satellite2)
	(power_avail satellite2)
	(pointing satellite2 phenomenon5)
	(supports instrument14 thermograph1)
	(supports instrument14 infrared2)
	(calibration_target instrument14 groundstation4)
	(supports instrument15 infrared2)
	(calibration_target instrument15 star1)
	(supports instrument16 image4)
	(supports instrument16 spectrograph6)
	(calibration_target instrument16 star2)
	(supports instrument17 image4)
	(supports instrument17 infrared7)
	(supports instrument17 image5)
	(calibration_target instrument17 groundstation0)
	(supports instrument18 image4)
	(supports instrument18 spectrograph6)
	(calibration_target instrument18 star2)
	(supports instrument19 infrared3)
	(supports instrument19 infrared7)
	(supports instrument19 spectrograph6)
	(calibration_target instrument19 star3)
	(supports instrument20 infrared3)
	(supports instrument20 infrared2)
	(calibration_target instrument20 star2)
	(supports instrument21 infrared2)
	(supports instrument21 thermograph1)
	(calibration_target instrument21 groundstation4)
	(on_board instrument14 satellite3)
	(on_board instrument15 satellite3)
	(on_board instrument16 satellite3)
	(on_board instrument17 satellite3)
	(on_board instrument18 satellite3)
	(on_board instrument19 satellite3)
	(on_board instrument20 satellite3)
	(on_board instrument21 satellite3)
	(power_avail satellite3)
	(pointing satellite3 phenomenon20)
	(supports instrument22 thermograph1)
	(supports instrument22 image5)
	(calibration_target instrument22 star2)
	(supports instrument23 infrared7)
	(supports instrument23 thermograph1)
	(calibration_target instrument23 star3)
	(supports instrument24 infrared3)
	(supports instrument24 spectrograph0)
	(calibration_target instrument24 groundstation0)
	(on_board instrument22 satellite4)
	(on_board instrument23 satellite4)
	(on_board instrument24 satellite4)
	(power_avail satellite4)
	(pointing satellite4 star14)
)
(:goal
	(and
		(pointing satellite0 planet17)
		(have_image phenomenon5 infrared7)
		(have_image phenomenon5 image4)
		(have_image phenomenon7 thermograph1)
		(have_image planet9 spectrograph0)
		(have_image planet9 spectrograph6)
		(have_image star10 infrared3)
		(have_image star10 spectrograph6)
		(have_image phenomenon11 infrared2)
		(have_image star12 spectrograph6)
		(have_image star12 thermograph1)
		(have_image phenomenon13 infrared7)
		(have_image phenomenon13 infrared2)
		(have_image star14 infrared2)
		(have_image planet15 infrared2)
		(have_image star16 image4)
		(have_image planet17 image5)
		(have_image planet17 image4)
		(have_image star18 infrared2)
		(have_image star19 infrared3)
		(have_image star19 thermograph1)
		(have_image phenomenon20 spectrograph0)
		(have_image planet21 infrared3)
		(have_image planet21 image5)
		(have_image planet22 infrared2)
		(have_image star23 infrared2)
		(have_image phenomenon24 spectrograph6)
		(have_image phenomenon24 image5)
	)
)
)