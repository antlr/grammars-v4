(define (domain rtam)
(:requirements :typing :durative-actions)
  (:types  
     ambulance police_car tow_truck fire_brigade - vehicle
     acc_victim vehicle car - subject
     city_location city - location
     accident_location hospital police_station garage fire_station - city_location
     route
     accident)

  (:predicates
    (at ?physical_obj1 - subject ?location1 - location)
    (available ?vehicle1 - vehicle)
    (busy ?vehicle1 - vehicle)
    (waiting ?subject1 - subject)
    (certified ?subject1 - subject)
    (aided ?subject1 - acc_victim)
    (uncertified ?subject1 - subject)
    (delivered ?subject1 - subject)
    (loaded ?subject1 - subject ?vehicle1 - vehicle)
    (identified ?accident1 - accident)
    (vehicle_involve ?vehicle1 - vehicle)
    (connects ?route1 - route ?location1 - location ?location2 - location)
    (in_city ?location1 - location ?city1 - city)
    (route_available ?route1 - route)
    (trapped ?hum - acc_victim)
    (untrapped ?hum - acc_victim)
    (on_fire ?car_acc - car)
    (off_fire ?car_acc - car)
  )

(:functions 
          ; (distance ?O - location ?L - location)
           (route-length ?O - route)
	 ;  (confirmation-time)
	 ;  (firstaid-time)
	   (speed ?V - vehicle)
	 ;  (loading-time)
	 ;  (loading-time-car)
         ;  (unloading-time)
         ;  (delivery-time)
	 ;  (untrapping-time)
	 ;  (extinguishing-time)
            )

  (:durative-action confirm_accident
       :parameters (?V - police_car ?P - subject ?A - accident_location)
       :duration (= ?duration 10)
       :condition (and 
	    (at start (at ?V ?A))
            (at start (at ?P ?A))
            (at start (uncertified ?P))
       )
       :effect (and 
            (at start (not (uncertified ?P)))
            (at end (waiting ?P))
            (at end (certified ?P))
        )
    )

(:durative-action untrap
       :parameters (?V - fire_brigade ?P - acc_victim ?A - accident_location)
	:duration (= ?duration 25)
       :condition (and 
            (at start (at ?P ?A))
            (at start (at ?V ?A))
            (at start (certified ?P))
	    (at start (available ?V))
            (at start (waiting ?P))
	    (at start (trapped ?P))
       )
       :effect (and 
	    (at start (not (available ?V)))
            (at end (not (trapped ?P)))
	    (at end (untrapped ?P))
	    (at end (available ?V))
        )
    )

(:durative-action extinguish_fire
       :parameters (?V - fire_brigade ?P - car ?A - accident_location)
	:duration (= ?duration 20)
       :condition (and 
            (at start (at ?P ?A))
            (at start (at ?V ?A))
	    (at start (available ?V))
            (at start (certified ?P))
            (at start (waiting ?P))
	    (at start (on_fire ?P))
       )
       :effect (and 
	    (at start (not (available ?V)))
            (at end (not (on_fire ?P)))
            (at end (off_fire ?P))
	    (at end (available ?V))
        )
    )

  (:durative-action first_aid
       :parameters (?V - ambulance ?P - acc_victim ?A - accident_location )
	:duration (= ?duration 20)
       :condition (and 
            (at start (at ?P ?A))
            (at start (at ?V ?A))
            (at start (certified ?P))
            (at start (waiting ?P))
	    (at start (untrapped ?P))
       )
       :effect (and
	    (at start (not (waiting ?P)))
            (at end (waiting ?P))
            (at end (aided ?P))
        )
    )

  (:durative-action load_victim
       :parameters ( ?V - ambulance ?L - accident_location ?P - acc_victim)
	:duration (= ?duration 5)
       :condition (and 
            (at start (at ?V ?L))
            (at start (at ?P ?L))
            (at start (certified ?P))
            (at start (waiting ?P))
            (at start (aided ?P))
	    (at start (available ?V))
       )
       :effect (and 
            (at start (not (available ?V)))
            (at start (busy ?V))
            (at start (not (waiting ?P)))
	    (at start (not (at ?P ?L)))
            (at end (loaded ?P ?V))
        )
    )

  (:durative-action move
       :parameters ( ?V - vehicle ?O - location ?City - city ?L - location ?City1 - city ?R - route)
       :duration (= ?duration (/ (route-length ?R) (speed ?V)))
       :condition (and 
			(at start (at ?V ?O))
            		(at start (in_city ?O ?City))
            		(at start (in_city ?L ?City1))
            		(at start (connects ?R ?City ?City1))
       )
       :effect (and 
		  (at start (not (at ?V ?O)))
                  (at end (at ?V ?L))
        )
    )

;  (:durative-action move_in_city
;       :parameters ( ?V - vehicle ?O - location ?City - city ?L - location)
;       :duration(=?duration(/(distance ?O ?L) (speed ?V)))
;       :condition (and 
;            (at start (at ?V ?O))
;            (at start (in_city ?O ?City))
;            (at start (in_city ?L ?City))
;       )
;       :effect (and 
;            (at start (not (at ?V ?O)))
;            (at end (at ?V ?L))
;        )
;    )

  (:durative-action load_car
       :parameters ( ?V - tow_truck ?L - accident_location ?P - car)
       :duration (= ?duration 5)
       :condition (and 
            (at start (at ?V ?L))
            (at start (at ?P ?L))
            (at start (waiting ?P))
            (at start (certified ?P))
	    (at start (available ?V))
	    (at start (off_fire ?P))
       )
       :effect (and 
            (at start (not (available ?V)))
            (at start (busy ?V))
            (at start (not (waiting ?P)))
	    (at start (not (at ?P ?L)))
            (at end (loaded ?P ?V))
        )
    )

  (:durative-action unload_car
       :parameters ( ?P - car ?L - garage ?V - tow_truck )
       :duration (= ?duration 5)
       :condition (and 
       	    (at start (at ?V ?L))
            (at start (loaded ?P ?V))
	    (at start (busy ?V))
       )
       :effect (and 
            (at start (not (loaded ?P ?V)))
	    (at end (at ?P ?L))
            (at start (not (busy ?V)))
            (at end (waiting ?P))
            (at end (available ?V))
        )
    )

  (:durative-action unload_victim
       :parameters ( ?P - acc_victim ?L - hospital ?V - ambulance)
       :duration (= ?duration 5)
       :condition (and 
       	    (at start (at ?V ?L))
            (at start (loaded ?P ?V))
            (at start (certified ?P))
            (at start (aided ?P))
	    (at start (busy ?V))	    
       )
       :effect (and 
            (at start (not (loaded ?P ?V)))
	    (at end (at ?P ?L))
            (at end (not (busy ?V)))
            (at end (waiting ?P))
            (at end (available ?V))
        )
    )

  (:durative-action deliver_victim
       :parameters ( ?P - acc_victim ?L - hospital )
       :duration (= ?duration 10)
       :condition (and 
            (at start (at ?P ?L))
            (at start (waiting ?P))
            (at start (certified ?P))
            (at start (aided ?P))
       )
       :effect (and 
            (at start (not (waiting ?P)))
            (at start (not (certified ?P)))
            (at start (not (aided ?P)))
            (at end (delivered ?P))
        )
    )

  (:durative-action deliver_vehicle
       :parameters ( ?P - car ?L - garage )
       :duration (= ?duration 10)
       :condition (and 
            (at start (at ?P ?L))
            (at start (waiting ?P))
            (at start (certified ?P))
       )
       :effect (and 
            (at start (not (waiting ?P)))
            (at start (not (certified ?P)))
            (at end (delivered ?P))
        )
    )

)
