(define (problem strips-sat-x-1) (:domain satellite)
(:objects
	groundstation1 - direction
	thermograph2 - mode
	infrared0 - mode
	infrared1 - mode
	planet5 - direction
	star4 - direction
	star7 - direction
	star6 - direction
	star0 - direction
	planet3 - direction
	star2 - direction
	satellite1 - satellite
	phenomenon8 - direction
	phenomenon9 - direction

	(:private satellite0
		instrument0 - instrument
		satellite0 - satellite
	)

	(:private satellite1
		instrument2 - instrument
		instrument1 - instrument
	)
)
(:init
	(supports instrument0 thermograph2)
	(supports instrument0 infrared0)
	(calibration_target instrument0 star0)
	(on_board instrument0 satellite0)
	(power_avail satellite0)
	(pointing satellite0 star6)
	(supports instrument1 infrared0)
	(supports instrument1 thermograph2)
	(supports instrument1 infrared1)
	(calibration_target instrument1 star2)
	(supports instrument2 thermograph2)
	(supports instrument2 infrared1)
	(calibration_target instrument2 star2)
	(on_board instrument1 satellite1)
	(on_board instrument2 satellite1)
	(power_avail satellite1)
	(pointing satellite1 star0)
)
(:goal
	(and
		(pointing satellite1 planet5)
		(have_image planet3 infrared1)
		(have_image star4 infrared1)
		(have_image planet5 thermograph2)
		(have_image star6 infrared1)
		(have_image star7 infrared0)
		(have_image phenomenon8 thermograph2)
		(have_image phenomenon9 infrared0)
	)
)
)