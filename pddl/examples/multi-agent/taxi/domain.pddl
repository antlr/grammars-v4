(define (domain taxi)
 
 (:requirements :strips :typing :multi-agent :unfactored-privacy)
 
 (:types
  location agent - object taxi passenger - agent
 )
 
 (:predicates 
  (directly-connected ?l1 - location ?l2 - location)     ;; location ?l2 is directly accessible from location ?l1.
  (at     ?a - agent  ?l - location)  ;; agent (taxi or passenger) ?a is at location ?l.
  (in     ?p - passenger ?t - taxi)   ;; passenger ?p is in taxi ?t.
  (empty    ?t - taxi)       ;; taxi ?t is empty.
  (free    ?l - location)      ;; location ?l is free, i.e. contains no taxi.
  
  (:private   ?p - passenger
   (goal-of  ?p - passenger ?l - location)  ;; the goal of passenger ?p is to reach location ?l, which is private information to ?p.
  )
  
 )

 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ;; taxi ?t drives (with or without a passenger) from location ?from to location ?to.
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 (:action drive
    :agent    ?t - taxi 
    :parameters  (?from - location ?to - location)
    :precondition  (and  (at ?t ?from)
        (directly-connected ?from ?to)
        (free ?to)
      )
    :effect    (and (not (at ?t ?from))
        (not (free ?to))
        (at ?t ?to)
        (free ?from)
      )
 )
 
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ;; passenger ?p enters taxi ?t at location ?l.
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 (:action enter
    :agent    ?p - passenger
    :parameters   (?t - taxi ?l - location)
    :precondition  (and  (at ?p ?l)
        (at ?t ?l)
        (empty ?t)
      )
    :effect    (and (not (empty ?t))
        (not (at ?p ?l))
        (in ?p ?t)
      )
 )

 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ;; passenger ?p exits taxi ?t at location ?l (at the desired goal destination of ?p).
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 (:action exit
    :agent    ?p - passenger
    :parameters   (?t - taxi ?l - location)
    :precondition  (and (in ?p ?t)
        (at ?t ?l)
        (goal-of ?p ?l)
      )
    :effect    (and (not (in ?p ?t))
        (empty ?t)
        (at ?p ?l)
      )
 )

)
