(define (problem roverprob2435) (:domain rover)
(:objects
	high_res - mode
	objective1 - objective
	objective0 - objective
	low_res - mode
	objective2 - objective
	waypoint2 - waypoint
	waypoint3 - waypoint
	waypoint0 - waypoint
	waypoint1 - waypoint
	general - lander
	colour - mode

	(:private rover0
		rover0 - rover
		rover0store - store
		camera2 - camera
	)

	(:private rover1
		camera0 - camera
		camera1 - camera
		rover1 - rover
		rover1store - store
	)
)
(:init
	(visible waypoint0 waypoint2)
	(visible waypoint2 waypoint0)
	(visible waypoint1 waypoint0)
	(visible waypoint0 waypoint1)
	(visible waypoint1 waypoint3)
	(visible waypoint3 waypoint1)
	(visible waypoint2 waypoint1)
	(visible waypoint1 waypoint2)
	(visible waypoint3 waypoint0)
	(visible waypoint0 waypoint3)
	(visible waypoint3 waypoint2)
	(visible waypoint2 waypoint3)
	(at_rock_sample waypoint0)
	(at_soil_sample waypoint1)
	(at_rock_sample waypoint1)
	(at_soil_sample waypoint2)
	(at_soil_sample waypoint3)
	(at_lander general waypoint3)
	(channel_free general)
	(at rover0 waypoint0)
	(available rover0)
	(store_of rover0store rover0)
	(empty rover0store)
	(equipped_for_rock_analysis rover0)
	(equipped_for_imaging rover0)
	(can_traverse rover0 waypoint0 waypoint1)
	(can_traverse rover0 waypoint1 waypoint0)
	(can_traverse rover0 waypoint0 waypoint3)
	(can_traverse rover0 waypoint3 waypoint0)
	(at rover1 waypoint0)
	(available rover1)
	(store_of rover1store rover1)
	(empty rover1store)
	(equipped_for_soil_analysis rover1)
	(equipped_for_imaging rover1)
	(can_traverse rover1 waypoint0 waypoint1)
	(can_traverse rover1 waypoint1 waypoint0)
	(can_traverse rover1 waypoint1 waypoint2)
	(can_traverse rover1 waypoint2 waypoint1)
	(can_traverse rover1 waypoint1 waypoint3)
	(can_traverse rover1 waypoint3 waypoint1)
	(on_board camera0 rover1)
	(calibration_target camera0 objective1)
	(supports camera0 high_res)
	(supports camera0 low_res)
	(on_board camera1 rover1)
	(calibration_target camera1 objective1)
	(supports camera1 colour)
	(supports camera1 high_res)
	(on_board camera2 rover0)
	(calibration_target camera2 objective1)
	(supports camera2 colour)
	(supports camera2 high_res)
	(supports camera2 low_res)
	(visible_from objective0 waypoint0)
	(visible_from objective0 waypoint1)
	(visible_from objective0 waypoint2)
	(visible_from objective0 waypoint3)
	(visible_from objective1 waypoint0)
	(visible_from objective1 waypoint1)
	(visible_from objective1 waypoint2)
	(visible_from objective2 waypoint0)
	(visible_from objective2 waypoint1)
	(visible_from objective2 waypoint2)
)
(:goal
	(and
		(communicated_soil_data waypoint1)
		(communicated_soil_data waypoint2)
		(communicated_rock_data waypoint0)
		(communicated_rock_data waypoint1)
		(communicated_image_data objective0 high_res)
		(communicated_image_data objective2 high_res)
		(communicated_image_data objective0 colour)
	)
)
)