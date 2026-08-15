(define (problem strips-sat-x-1) (:domain satellite)
(:objects
	star4 - direction
	infrared0 - mode
	phenomenon6 - direction
	phenomenon7 - direction
	phenomenon5 - direction
	star1 - direction
	star0 - direction
	star3 - direction
	star2 - direction
	spectrograph2 - mode
	image1 - mode
	satellite0 - satellite

	(:private satellite0
		instrument2 - instrument
		instrument0 - instrument
		instrument1 - instrument
	)

	(:private satellite1
		instrument3 - instrument
		satellite1 - satellite
	)
)
(:init
	(supports instrument0 spectrograph2)
	(supports instrument0 infrared0)
	(calibration_target instrument0 star1)
	(supports instrument1 image1)
	(calibration_target instrument1 star2)
	(supports instrument2 infrared0)
	(supports instrument2 image1)
	(calibration_target instrument2 star0)
	(on_board instrument0 satellite0)
	(on_board instrument1 satellite0)
	(on_board instrument2 satellite0)
	(power_avail satellite0)
	(pointing satellite0 star4)
	(supports instrument3 spectrograph2)
	(supports instrument3 infrared0)
	(supports instrument3 image1)
	(calibration_target instrument3 star0)
	(on_board instrument3 satellite1)
	(power_avail satellite1)
	(pointing satellite1 star0)
)
(:goal
	(and
		(pointing satellite0 phenomenon5)
		(have_image star3 infrared0)
		(have_image star4 spectrograph2)
		(have_image phenomenon5 spectrograph2)
		(have_image phenomenon7 spectrograph2)
	)
)
)