(define (domain wireless)
 
 (:requirements :strips :typing :multi-agent :unfactored-privacy)
 
 (:types
  node level message - object base sensor - node
 )

 (:constants
  Zero Low Normal High - level
 )

 (:predicates
  (neighbor   ?n1 - node ?n2 - node)          ;; node ?n1 and ?n2 are neighbors.
  (has-data   ?n - node ?s - sensor)        ;; node ?n has measurement data about the location of sensor node ?s.      
  (higher    ?l1 - level ?l2 - level)         ;; level ?l1 is higher than level ?l2.
  (next    ?l1 - level ?l2 - level)         ;; level ?l1 is next to (and higher than) level ?l2.
  (is-message-at  ?m - message  ?n - node)      ;; message ?m is at node ?n.
  (message-data  ?m - message  ?s - sensor)     ;; message ?m has data about the location of sensor node ?s.
  (sending   ?from - sensor  ?to - node ?m - message)  ;; sensor node ?from is sending message ?m to node ?to
  
  (:private   ?s - sensor
   (energy   ?s - sensor   ?lv - level)     ;; current energy level of sensor node ?s is ?lv, privately known only to ?s.
  )
 
 )

 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ;; sensor node ?s generates measurement data about its location, and as a result the energy level of ?s changes from ?e0 to ?e1.
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 (:action generate-data
    :agent    ?s - sensor
    :parameters  (?e0 - level ?e1 - level)
    :precondition  (and
       (energy ?s ?e0)
       (higher ?e0 Zero)
       (next ?e0 ?e1)
      )
    :effect   (and
       (not (energy ?s ?e0))
       (energy ?s ?e1)
       (has-data ?s ?s)
      )
 )

 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ;; sensor node ?s1 adds the measurement data about the location of sensor node ?s2 to message ?m.
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 (:action add-to-message
    :agent    ?s1 - sensor
    :parameters  (?s2 - sensor ?m - message)
    :precondition  (and
       (has-data ?s1 ?s2)
       (is-message-at ?m ?s1)
       (not (message-data ?m ?s2))
      )
    :effect   (and
       (not (has-data ?s1 ?s2))
       (message-data ?m ?s2)
      )
 )

 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ;; sensor node ?sender sends message ?m to node ?receiver, and as a result the energy level of ?sender changes from ?e0 to ?e1.
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 (:action send-message
    :agent    ?sender - sensor
    :parameters  (?receiver - node ?m - message ?e0 - level ?e1 - level)
    :precondition  (and
       (energy ?sender ?e0)
       (higher ?e0 Zero)
       (next ?e0 ?e1)
       (neighbor ?sender ?receiver)
       (is-message-at ?m ?sender)
       (not (is-message-at ?m ?receiver))
       (not (sending ?sender ?receiver ?m))
      )
    :effect   (and
       (not (energy ?sender ?e0))
       (not (is-message-at ?m ?sender))
       (energy ?sender ?e1)
       (sending ?sender ?receiver ?m)
      )
 )

 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ;; node ?receiver receives message ?m from sensor node ?sender.
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 (:action receive-message
    :agent    ?receiver - node
    :parameters  (?sender - sensor ?m - message)
    :precondition  (and
       (not (is-message-at ?m ?receiver))
       (sending ?sender ?receiver ?m)
      )
    :effect   (and
       (not (sending ?sender ?receiver ?m))
       (is-message-at ?m ?receiver)
      )
 )

 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ;; node ?n gets the measurement data about the location of sensor node ?s from message ?m.
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 (:action get-data-from-message
    :agent    ?n - node
    :parameters  (?s - sensor ?m - message)
    :precondition (and
       (is-message-at ?m ?n)
       (message-data ?m ?s)
      )
    :effect   (and
       (not (message-data ?m ?s))
       (has-data ?n ?s)
      )
 )

)

