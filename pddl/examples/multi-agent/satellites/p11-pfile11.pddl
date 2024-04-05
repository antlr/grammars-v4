(define (problem strips-sat-x-1) (:domain satellite)
(:objects
	planet13 - direction
	planet14 - direction
	infrared1 - mode
	planet16 - direction
	planet19 - direction
	spectrograph4 - mode
	phenomenon15 - direction
	phenomenon7 - direction
	planet6 - direction
	image3 - mode
	satellite1 - satellite
	satellite0 - satellite
	satellite4 - satellite
	star17 - direction
	star11 - direction
	star10 - direction
	star12 - direction
	thermograph2 - mode
	infrared0 - mode
	star18 - direction
	star5 - direction
	star4 - direction
	star1 - direction
	star0 - direction
	star2 - direction
	phenomenon9 - direction
	groundstation3 - direction
	star8 - direction

	(:private satellite0
		instrument0 - instrument
	)

	(:private satellite1
		instrument2 - instrument
		instrument3 - instrument
		instrument1 - instrument
	)

	(:private satellite2
		satellite2 - satellite
		instrument6 - instrument
		instrument4 - instrument
		instrument5 - instrument
	)

	(:private satellite3
		satellite3 - satellite
		instrument7 - instrument
	)

	(:private satellite4
		instrument8 - instrument
	)
)
(:init
	(supports instrument0 spectrograph4)
	(calibration_target instrument0 star0)
	(on_board instrument0 satellite0)
	(power_avail satellite0)
	(pointing satellite0 star8)
	(supports instrument1 infrared0)
	(supports instrument1 infrared1)
	(calibration_target instrument1 groundstation3)
	(supports instrument2 infrared1)
	(supports instrument2 infrared0)
	(calibration_target instrument2 star2)
	(supports instrument3 spectrograph4)
	(supports instrument3 infrared1)
	(supports instrument3 thermograph2)
	(calibration_target instrument3 star0)
	(on_board instrument1 satellite1)
	(on_board instrument2 satellite1)
	(on_board instrument3 satellite1)
	(power_avail satellite1)
	(pointing satellite1 groundstation3)
	(supports instrument4 infrared1)
	(supports instrument4 image3)
	(supports instrument4 infrared0)
	(calibration_target instrument4 star2)
	(supports instrument5 thermograph2)
	(supports instrument5 spectrograph4)
	(calibration_target instrument5 star0)
	(supports instrument6 infrared0)
	(calibration_target instrument6 groundstation3)
	(on_board instrument4 satellite2)
	(on_board instrument5 satellite2)
	(on_board instrument6 satellite2)
	(power_avail satellite2)
	(pointing satellite2 star4)
	(supports instrument7 image3)
	(calibration_target instrument7 star2)
	(on_board instrument7 satellite3)
	(power_avail satellite3)
	(pointing satellite3 phenomenon9)
	(supports instrument8 infrared0)
	(supports instrument8 spectrograph4)
	(supports instrument8 infrared1)
	(calibration_target instrument8 star2)
	(on_board instrument8 satellite4)
	(power_avail satellite4)
	(pointing satellite4 phenomenon9)
)
(:goal
	(and
		(pointing satellite0 phenomenon9)
		(pointing satellite1 star4)
		(pointing satellite4 star11)
		(have_image star5 image3)
		(have_image planet6 infrared1)
		(have_image phenomenon7 infrared1)
		(have_image star8 image3)
		(have_image star10 thermograph2)
		(have_image star11 infrared1)
		(have_image planet13 spectrograph4)
		(have_image planet14 thermograph2)
		(have_image phenomenon15 infrared0)
		(have_image planet16 image3)
		(have_image star17 infrared0)
	)
)
)