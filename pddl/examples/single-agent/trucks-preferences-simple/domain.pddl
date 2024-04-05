; IPC5 Domain: Trucks SimplePreferences
; Authors: Yannis Dimopoulos, Alfonso Gerevini and Alessandro Saetti 

(define (domain Trucks-SimplePreferences) 
(:requirements :typing :adl :preferences)  

(:types truckarea time location locatable - object
        truck package - locatable) 

(:predicates (at ?x - locatable ?l - location) 	     
             (in ?p - package ?t - truck ?a - truckarea) 	     
             (connected ?x ?y - location)
             (free ?a - truckarea ?t - truck)
 	     (time-now ?t - time)
 	     (next ?t1 - time ?t2 - time)
	     (le ?t1 - time ?t2 - time)
 	     (delivered ?p - package ?l - location ?t - time)
	     (at-destination ?p - package ?l - location)
 	     (closer ?a1 - truckarea ?a2 - truckarea))

(:action load
 :parameters (?p - package ?t - truck ?a1 - truckarea ?l - location)
 :precondition (and (at ?t ?l) (at ?p ?l) (free ?a1 ?t)
  		    (forall (?a2 - truckarea)
  			    (imply (closer ?a2 ?a1) (free ?a2 ?t))))
 :effect (and (not (at ?p ?l)) (not (free ?a1 ?t)) (in ?p ?t ?a1)))

(:action unload
 :parameters (?p - package ?t - truck ?a1 - truckarea ?l - location)
 :precondition (and (at ?t ?l) (in ?p ?t ?a1)
  		    (forall (?a2 - truckarea)
  			    (imply (closer ?a2 ?a1) (free ?a2 ?t))))
 :effect (and (not (in ?p ?t ?a1)) (free ?a1 ?t) (at ?p ?l)))

(:action drive
 :parameters (?t - truck ?from ?to - location ?t1 ?t2 - time)
 :precondition (and (at ?t ?from) (connected ?from ?to) 
		    (time-now ?t1) (next ?t1 ?t2))
 :effect (and (not (at ?t ?from)) (not (time-now ?t1)) 
	      (time-now ?t2) (at ?t ?to)))

(:action deliver
 :parameters (?p - package ?l - location ?t1 ?t2 - time)
 :precondition (and (at ?p ?l) (time-now ?t1) (le ?t1 ?t2))
 :effect (and (not (at ?p ?l)) (delivered ?p ?l ?t2) (at-destination ?p ?l)))

) 

