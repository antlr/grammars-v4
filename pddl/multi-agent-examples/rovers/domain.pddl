(define (domain rover)
	(:requirements :typing :multi-agent :unfactored-privacy)
(:types
	rover waypoint store camera mode lander objective - object
)
(:predicates
	;;public
	(visible ?w - waypoint ?p - waypoint)
	(visible_from ?o - objective ?w - waypoint)

	;;public because public in some problems
	(at_rock_sample ?w - waypoint)
	(at_soil_sample ?w - waypoint)
	(at_lander ?x - lander ?y - waypoint)
	(communicated_image_data ?o - objective ?m - mode)
        (communicated_rock_data ?w - waypoint)
	(communicated_soil_data ?w - waypoint)

	;;public because shared among all agents without reference to an agent
	(empty ?s - store)
	(full ?s - store)
	(supports ?c - camera ?m - mode)
	(calibration_target ?i - camera ?o - objective)
	(channel_free ?l - lander)

	(:private ?agent - rover
		(at ?agent - rover ?y - waypoint)
		(can_traverse ?agent - rover ?x - waypoint ?y - waypoint)
		(equipped_for_soil_analysis ?agent - rover)
		(equipped_for_rock_analysis ?agent - rover)
		(equipped_for_imaging ?agent - rover)
		(have_rock_analysis ?agent - rover ?w - waypoint)
		(have_soil_analysis ?agent - rover ?w - waypoint)
		(calibrated ?c - camera ?agent - rover)
		(available ?agent - rover)
		(have_image ?agent - rover ?o - objective ?m - mode)
		(store_of ?s - store ?agent - rover)
		(on_board ?i - camera ?agent - rover)
	)
)

(:action navigate
	:agent ?x - rover
	:parameters (?y - waypoint ?z - waypoint)
	:precondition (and
		(can_traverse ?x ?y ?z)
		(available ?x)
		(at ?x ?y)
		(visible ?y ?z)
	)
	:effect (and
		(not (at ?x ?y))
		(at ?x ?z)
	)
)


(:action sample_soil
	:agent ?x - rover
	:parameters (?s - store ?p - waypoint)
	:precondition (and
		(at ?x ?p)
		(at_soil_sample ?p)
		(equipped_for_soil_analysis ?x)
		(store_of ?s ?x)
		(empty ?s)
	)
	:effect (and
		(not (empty ?s))
		(full ?s)
		(have_soil_analysis ?x ?p)
		(not (at_soil_sample ?p))
	)
)


(:action sample_rock
	:agent ?x - rover
	:parameters (?s - store ?p - waypoint)
	:precondition (and
		(at ?x ?p)
		(at_rock_sample ?p)
		(equipped_for_rock_analysis ?x)
		(store_of ?s ?x)
		(empty ?s)
	)
	:effect (and
		(not (empty ?s))
		(full ?s)
		(have_rock_analysis ?x ?p)
		(not (at_rock_sample ?p))
	)
)


(:action drop
	:agent ?x - rover
	:parameters (?y - store)
	:precondition (and
		(store_of ?y ?x)
		(full ?y)
	)
	:effect (and
		(not (full ?y))
		(empty ?y)
	)
)


(:action calibrate
	:agent ?r - rover
	:parameters (?i - camera ?t - objective ?w - waypoint)
	:precondition (and
		(equipped_for_imaging ?r)
		(calibration_target ?i ?t)
		(at ?r ?w)
		(visible_from ?t ?w)
		(on_board ?i ?r)
	)
	:effect 
		(calibrated ?i ?r)
)


(:action take_image
	:agent ?r - rover
	:parameters (?p - waypoint ?o - objective ?i - camera ?m - mode)
	:precondition (and
		(calibrated ?i ?r)
		(on_board ?i ?r)
		(equipped_for_imaging ?r)
		(supports ?i ?m)
		(visible_from ?o ?p)
		(at ?r ?p)
	)
	:effect (and
		(have_image ?r ?o ?m)
		(not (calibrated ?i ?r))
	)
)


(:action communicate_soil_data
	:agent ?r - rover
	:parameters (?l - lander ?p - waypoint ?x - waypoint ?y - waypoint)
	:precondition (and
		(at ?r ?x)
		(at_lander ?l ?y)
		(have_soil_analysis ?r ?p)
		(visible ?x ?y)
		(available ?r)
		(channel_free ?l)
	)
	:effect (and
		(not (available ?r))
		(not (channel_free ?l))
		(channel_free ?l)
		(communicated_soil_data ?p)
		(available ?r)
	)
)


(:action communicate_rock_data
	:agent ?r - rover
	:parameters (?l - lander ?p - waypoint ?x - waypoint ?y - waypoint)
	:precondition (and
		(at ?r ?x)
		(at_lander ?l ?y)
		(have_rock_analysis ?r ?p)
		(visible ?x ?y)
		(available ?r)
		(channel_free ?l)
	)
	:effect (and
		(not (available ?r))
		(not (channel_free ?l))
		(channel_free ?l)
		(communicated_rock_data ?p)
		(available ?r)
	)
)


(:action communicate_image_data
	:agent ?r - rover
	:parameters (?l - lander ?o - objective ?m - mode ?x - waypoint ?y - waypoint)
	:precondition (and
		(at ?r ?x)
		(at_lander ?l ?y)
		(have_image ?r ?o ?m)
		(visible ?x ?y)
		(available ?r)
		(channel_free ?l)
	)
	:effect (and
		(not (available ?r))
		(not (channel_free ?l))
		(channel_free ?l)
		(communicated_image_data ?o ?m)
		(available ?r)
	)
)

)
