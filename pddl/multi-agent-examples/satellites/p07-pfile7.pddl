(define (problem strips-sat-x-1) (:domain satellite)
(:objects
	planet11 - direction
	planet10 - direction
	phenomenon5 - direction
	image2 - mode
	image3 - mode
	satellite2 - satellite
	image1 - mode
	image0 - mode
	planet9 - direction
	satellite1 - satellite
	planet8 - direction
	star7 - direction
	star6 - direction
	star1 - direction
	star3 - direction
	groundstation4 - direction
	groundstation2 - direction
	groundstation0 - direction

	(:private satellite0
		instrument2 - instrument
		instrument0 - instrument
		instrument1 - instrument
		satellite0 - satellite
	)

	(:private satellite1
		instrument3 - instrument
	)

	(:private satellite2
		instrument4 - instrument
		instrument5 - instrument
	)

	(:private satellite3
		satellite3 - satellite
		instrument6 - instrument
		instrument7 - instrument
	)
)
(:init
	(supports instrument0 image1)
	(supports instrument0 image3)
	(calibration_target instrument0 star1)
	(supports instrument1 image3)
	(calibration_target instrument1 groundstation0)
	(supports instrument2 image0)
	(calibration_target instrument2 groundstation2)
	(on_board instrument0 satellite0)
	(on_board instrument1 satellite0)
	(on_board instrument2 satellite0)
	(power_avail satellite0)
	(pointing satellite0 star6)
	(supports instrument3 image0)
	(supports instrument3 image2)
	(calibration_target instrument3 groundstation4)
	(on_board instrument3 satellite1)
	(power_avail satellite1)
	(pointing satellite1 groundstation0)
	(supports instrument4 image1)
	(supports instrument4 image0)
	(calibration_target instrument4 star1)
	(supports instrument5 image2)
	(supports instrument5 image0)
	(supports instrument5 image1)
	(calibration_target instrument5 star1)
	(on_board instrument4 satellite2)
	(on_board instrument5 satellite2)
	(power_avail satellite2)
	(pointing satellite2 star6)
	(supports instrument6 image2)
	(supports instrument6 image1)
	(supports instrument6 image0)
	(calibration_target instrument6 groundstation4)
	(supports instrument7 image3)
	(supports instrument7 image0)
	(supports instrument7 image1)
	(calibration_target instrument7 groundstation0)
	(on_board instrument6 satellite3)
	(on_board instrument7 satellite3)
	(power_avail satellite3)
	(pointing satellite3 groundstation2)
)
(:goal
	(and
		(pointing satellite1 star1)
		(pointing satellite2 phenomenon5)
		(have_image phenomenon5 image0)
		(have_image star6 image1)
		(have_image star7 image0)
		(have_image planet8 image0)
		(have_image planet9 image3)
		(have_image planet10 image0)
		(have_image planet11 image2)
	)
)
)