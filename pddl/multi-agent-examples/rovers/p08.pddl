(define (problem truck-crane-p1) (:domain rover)
(:objects
	objective2 - objective
	high_res - mode
	objective1 - objective
	objective0 - objective
	waypoint4 - waypoint
	waypoint5 - waypoint
	waypoint2 - waypoint
	waypoint3 - waypoint
	waypoint0 - waypoint
	waypoint1 - waypoint
	low_res - mode
	general - lander
	colour - mode

	(:private rover0
		rover0 - rover
		rover0store - store
	)

	(:private rover1
		camera2 - camera
		rover1 - rover
		rover1store - store
	)

	(:private rover2
		rover2 - rover
		rover2store - store
		camera3 - camera
	)

	(:private rover3
		camera0 - camera
		rover3store - store
		camera1 - camera
		rover3 - rover
	)
)
(:init
	(visible waypoint0 waypoint1)
	(visible waypoint1 waypoint0)
	(visible waypoint0 waypoint3)
	(visible waypoint3 waypoint0)
	(visible waypoint1 waypoint5)
	(visible waypoint5 waypoint1)
	(visible waypoint2 waypoint0)
	(visible waypoint0 waypoint2)
	(visible waypoint2 waypoint1)
	(visible waypoint1 waypoint2)
	(visible waypoint2 waypoint3)
	(visible waypoint3 waypoint2)
	(visible waypoint2 waypoint4)
	(visible waypoint4 waypoint2)
	(visible waypoint3 waypoint4)
	(visible waypoint4 waypoint3)
	(visible waypoint3 waypoint5)
	(visible waypoint5 waypoint3)
	(visible waypoint4 waypoint0)
	(visible waypoint0 waypoint4)
	(visible waypoint4 waypoint1)
	(visible waypoint1 waypoint4)
	(visible waypoint5 waypoint2)
	(visible waypoint2 waypoint5)
	(visible waypoint5 waypoint4)
	(visible waypoint4 waypoint5)
	(at_soil_sample waypoint1)
	(at_rock_sample waypoint2)
	(at_soil_sample waypoint3)
	(at_rock_sample waypoint3)
	(at_soil_sample waypoint4)
	(at_rock_sample waypoint4)
	(at_rock_sample waypoint5)
	(at_lander general waypoint0)
	(channel_free general)
	(at rover0 waypoint2)
	(available rover0)
	(store_of rover0store rover0)
	(empty rover0store)
	(equipped_for_soil_analysis rover0)
	(equipped_for_rock_analysis rover0)
	(can_traverse rover0 waypoint2 waypoint0)
	(can_traverse rover0 waypoint0 waypoint2)
	(can_traverse rover0 waypoint2 waypoint3)
	(can_traverse rover0 waypoint3 waypoint2)
	(can_traverse rover0 waypoint2 waypoint4)
	(can_traverse rover0 waypoint4 waypoint2)
	(can_traverse rover0 waypoint0 waypoint1)
	(can_traverse rover0 waypoint1 waypoint0)
	(can_traverse rover0 waypoint3 waypoint5)
	(can_traverse rover0 waypoint5 waypoint3)
	(at rover1 waypoint2)
	(available rover1)
	(store_of rover1store rover1)
	(empty rover1store)
	(equipped_for_rock_analysis rover1)
	(equipped_for_imaging rover1)
	(can_traverse rover1 waypoint2 waypoint0)
	(can_traverse rover1 waypoint0 waypoint2)
	(can_traverse rover1 waypoint2 waypoint3)
	(can_traverse rover1 waypoint3 waypoint2)
	(can_traverse rover1 waypoint2 waypoint5)
	(can_traverse rover1 waypoint5 waypoint2)
	(can_traverse rover1 waypoint0 waypoint1)
	(can_traverse rover1 waypoint1 waypoint0)
	(can_traverse rover1 waypoint3 waypoint4)
	(can_traverse rover1 waypoint4 waypoint3)
	(at rover2 waypoint2)
	(available rover2)
	(store_of rover2store rover2)
	(empty rover2store)
	(equipped_for_rock_analysis rover2)
	(equipped_for_imaging rover2)
	(can_traverse rover2 waypoint2 waypoint0)
	(can_traverse rover2 waypoint0 waypoint2)
	(can_traverse rover2 waypoint2 waypoint1)
	(can_traverse rover2 waypoint1 waypoint2)
	(can_traverse rover2 waypoint2 waypoint4)
	(can_traverse rover2 waypoint4 waypoint2)
	(can_traverse rover2 waypoint2 waypoint5)
	(can_traverse rover2 waypoint5 waypoint2)
	(can_traverse rover2 waypoint0 waypoint3)
	(can_traverse rover2 waypoint3 waypoint0)
	(at rover3 waypoint3)
	(available rover3)
	(store_of rover3store rover3)
	(empty rover3store)
	(equipped_for_soil_analysis rover3)
	(equipped_for_rock_analysis rover3)
	(equipped_for_imaging rover3)
	(can_traverse rover3 waypoint3 waypoint0)
	(can_traverse rover3 waypoint0 waypoint3)
	(can_traverse rover3 waypoint3 waypoint2)
	(can_traverse rover3 waypoint2 waypoint3)
	(can_traverse rover3 waypoint3 waypoint4)
	(can_traverse rover3 waypoint4 waypoint3)
	(can_traverse rover3 waypoint0 waypoint1)
	(can_traverse rover3 waypoint1 waypoint0)
	(can_traverse rover3 waypoint2 waypoint5)
	(can_traverse rover3 waypoint5 waypoint2)
	(on_board camera0 rover3)
	(calibration_target camera0 objective1)
	(supports camera0 colour)
	(supports camera0 high_res)
	(supports camera0 low_res)
	(on_board camera1 rover3)
	(calibration_target camera1 objective0)
	(supports camera1 colour)
	(supports camera1 high_res)
	(on_board camera2 rover1)
	(calibration_target camera2 objective0)
	(supports camera2 colour)
	(supports camera2 high_res)
	(supports camera2 low_res)
	(on_board camera3 rover2)
	(calibration_target camera3 objective1)
	(supports camera3 colour)
	(supports camera3 low_res)
	(visible_from objective0 waypoint0)
	(visible_from objective0 waypoint1)
	(visible_from objective0 waypoint2)
	(visible_from objective1 waypoint0)
	(visible_from objective2 waypoint0)
	(visible_from objective2 waypoint1)
	(visible_from objective2 waypoint2)
)
(:goal
	(and
		(communicated_soil_data waypoint1)
		(communicated_soil_data waypoint3)
		(communicated_soil_data waypoint4)
		(communicated_rock_data waypoint5)
		(communicated_rock_data waypoint4)
		(communicated_image_data objective0 low_res)
		(communicated_image_data objective0 high_res)
		(communicated_image_data objective2 low_res)
	)
)
)