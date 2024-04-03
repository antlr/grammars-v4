(define (problem roverprob2312) (:domain rover)
(:objects
	objective1 - objective
	objective0 - objective
	colour - mode
	waypoint5 - waypoint
	waypoint2 - waypoint
	waypoint3 - waypoint
	waypoint0 - waypoint
	waypoint1 - waypoint
	low_res - mode
	general - lander
	waypoint4 - waypoint

	(:private rover0
		camera0 - camera
		camera1 - camera
		rover0 - rover
		rover0store - store
	)

	(:private rover1
		rover1 - rover
		camera2 - camera
		rover1store - store
		high_res - mode
	)
)
(:init
	(visible waypoint0 waypoint3)
	(visible waypoint3 waypoint0)
	(visible waypoint1 waypoint0)
	(visible waypoint0 waypoint1)
	(visible waypoint1 waypoint4)
	(visible waypoint4 waypoint1)
	(visible waypoint2 waypoint0)
	(visible waypoint0 waypoint2)
	(visible waypoint2 waypoint3)
	(visible waypoint3 waypoint2)
	(visible waypoint3 waypoint1)
	(visible waypoint1 waypoint3)
	(visible waypoint3 waypoint5)
	(visible waypoint5 waypoint3)
	(visible waypoint4 waypoint3)
	(visible waypoint3 waypoint4)
	(visible waypoint5 waypoint0)
	(visible waypoint0 waypoint5)
	(visible waypoint5 waypoint2)
	(visible waypoint2 waypoint5)
	(visible waypoint5 waypoint4)
	(visible waypoint4 waypoint5)
	(at_rock_sample waypoint0)
	(at_soil_sample waypoint1)
	(at_soil_sample waypoint2)
	(at_rock_sample waypoint2)
	(at_soil_sample waypoint3)
	(at_rock_sample waypoint3)
	(at_soil_sample waypoint4)
	(at_soil_sample waypoint5)
	(at_rock_sample waypoint5)
	(at_lander general waypoint3)
	(channel_free general)
	(at rover0 waypoint1)
	(available rover0)
	(store_of rover0store rover0)
	(empty rover0store)
	(equipped_for_rock_analysis rover0)
	(equipped_for_imaging rover0)
	(can_traverse rover0 waypoint1 waypoint0)
	(can_traverse rover0 waypoint0 waypoint1)
	(can_traverse rover0 waypoint1 waypoint4)
	(can_traverse rover0 waypoint4 waypoint1)
	(can_traverse rover0 waypoint0 waypoint2)
	(can_traverse rover0 waypoint2 waypoint0)
	(can_traverse rover0 waypoint0 waypoint3)
	(can_traverse rover0 waypoint3 waypoint0)
	(can_traverse rover0 waypoint0 waypoint5)
	(can_traverse rover0 waypoint5 waypoint0)
	(at rover1 waypoint4)
	(available rover1)
	(store_of rover1store rover1)
	(empty rover1store)
	(equipped_for_soil_analysis rover1)
	(equipped_for_imaging rover1)
	(can_traverse rover1 waypoint4 waypoint1)
	(can_traverse rover1 waypoint1 waypoint4)
	(can_traverse rover1 waypoint4 waypoint3)
	(can_traverse rover1 waypoint3 waypoint4)
	(can_traverse rover1 waypoint4 waypoint5)
	(can_traverse rover1 waypoint5 waypoint4)
	(can_traverse rover1 waypoint1 waypoint0)
	(can_traverse rover1 waypoint0 waypoint1)
	(can_traverse rover1 waypoint5 waypoint2)
	(can_traverse rover1 waypoint2 waypoint5)
	(on_board camera0 rover0)
	(calibration_target camera0 objective0)
	(supports camera0 colour)
	(supports camera0 low_res)
	(on_board camera1 rover0)
	(calibration_target camera1 objective1)
	(supports camera1 colour)
	(supports camera1 low_res)
	(on_board camera2 rover1)
	(calibration_target camera2 objective0)
	(supports camera2 colour)
	(supports camera2 high_res)
	(visible_from objective0 waypoint0)
	(visible_from objective0 waypoint1)
	(visible_from objective0 waypoint2)
	(visible_from objective0 waypoint3)
	(visible_from objective0 waypoint4)
	(visible_from objective0 waypoint5)
	(visible_from objective1 waypoint0)
	(visible_from objective1 waypoint1)
)
(:goal
	(and
		(communicated_soil_data waypoint5)
		(communicated_soil_data waypoint1)
		(communicated_soil_data waypoint4)
		(communicated_soil_data waypoint2)
		(communicated_rock_data waypoint0)
		(communicated_rock_data waypoint2)
		(communicated_rock_data waypoint3)
		(communicated_image_data objective0 colour)
		(communicated_image_data objective1 low_res)
		(communicated_image_data objective0 low_res)
	)
)
)