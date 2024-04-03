(define (problem strips-sat-x-1) (:domain satellite)
(:objects
	planet11 - direction
	planet10 - direction
	planet5 - direction
	phenomenon7 - direction
	phenomenon6 - direction
	image3 - mode
	image2 - mode
	satellite0 - satellite
	phenomenon8 - direction
	image4 - mode
	star14 - direction
	satellite3 - satellite
	phenomenon12 - direction
	phenomenon13 - direction
	infrared1 - mode
	star4 - direction
	spectrograph0 - mode
	star0 - direction
	star3 - direction
	star2 - direction
	satellite4 - satellite
	star9 - direction
	groundstation1 - direction

	(:private satellite0
		instrument2 - instrument
		instrument0 - instrument
		instrument1 - instrument
	)

	(:private satellite1
		instrument3 - instrument
		satellite1 - satellite
		instrument4 - instrument
		instrument5 - instrument
	)

	(:private satellite2
		satellite2 - satellite
		instrument6 - instrument
	)

	(:private satellite3
		instrument7 - instrument
	)

	(:private satellite4
		instrument8 - instrument
		instrument9 - instrument
		instrument10 - instrument
	)
)
(:init
	(supports instrument0 infrared1)
	(supports instrument0 image4)
	(calibration_target instrument0 star3)
	(supports instrument1 image4)
	(supports instrument1 image2)
	(supports instrument1 spectrograph0)
	(calibration_target instrument1 star4)
	(supports instrument2 image2)
	(calibration_target instrument2 star2)
	(on_board instrument0 satellite0)
	(on_board instrument1 satellite0)
	(on_board instrument2 satellite0)
	(power_avail satellite0)
	(pointing satellite0 star0)
	(supports instrument3 image2)
	(supports instrument3 image3)
	(supports instrument3 image4)
	(calibration_target instrument3 star2)
	(supports instrument4 image3)
	(supports instrument4 image2)
	(calibration_target instrument4 star3)
	(supports instrument5 image4)
	(supports instrument5 infrared1)
	(supports instrument5 spectrograph0)
	(calibration_target instrument5 star3)
	(on_board instrument3 satellite1)
	(on_board instrument4 satellite1)
	(on_board instrument5 satellite1)
	(power_avail satellite1)
	(pointing satellite1 planet11)
	(supports instrument6 image2)
	(supports instrument6 spectrograph0)
	(calibration_target instrument6 star2)
	(on_board instrument6 satellite2)
	(power_avail satellite2)
	(pointing satellite2 phenomenon6)
	(supports instrument7 image3)
	(supports instrument7 spectrograph0)
	(supports instrument7 image4)
	(calibration_target instrument7 star0)
	(on_board instrument7 satellite3)
	(power_avail satellite3)
	(pointing satellite3 planet10)
	(supports instrument8 image4)
	(supports instrument8 infrared1)
	(supports instrument8 image3)
	(calibration_target instrument8 groundstation1)
	(supports instrument9 image4)
	(calibration_target instrument9 star0)
	(supports instrument10 image2)
	(supports instrument10 infrared1)
	(supports instrument10 image4)
	(calibration_target instrument10 star2)
	(on_board instrument8 satellite4)
	(on_board instrument9 satellite4)
	(on_board instrument10 satellite4)
	(power_avail satellite4)
	(pointing satellite4 star9)
)
(:goal
	(and
		(pointing satellite0 phenomenon7)
		(pointing satellite3 star9)
		(pointing satellite4 planet5)
		(have_image planet5 image2)
		(have_image phenomenon6 image3)
		(have_image phenomenon7 infrared1)
		(have_image phenomenon8 image2)
		(have_image star9 image3)
		(have_image planet10 image4)
		(have_image planet11 spectrograph0)
		(have_image phenomenon12 image3)
		(have_image phenomenon13 spectrograph0)
		(have_image star14 image4)
	)
)
)