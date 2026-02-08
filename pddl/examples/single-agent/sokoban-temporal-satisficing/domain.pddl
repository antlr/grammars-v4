(define (domain sokoban-temporal)
  (:requirements :typing :durative-actions)
  (:types
  thing - object
  location - object
  direction - object
  player - thing
  stone - thing
  )
  (:predicates (clear ?l - location)
	       (at ?t - thing ?l - location)
	       (at-goal ?s - stone)
	       (IS-GOAL ?l - location)
	       (IS-NONGOAL ?l - location)
               (MOVE-DIR ?from - location ?to - location ?dir - direction))

  (:durative-action move
   :parameters (?p - player ?from - location ?to - location ?dir - direction)
   :duration  (= ?duration 1)
   :condition (and (at start (at ?p ?from))
                   (at start (clear ?to))
                   (over all (MOVE-DIR ?from ?to ?dir))
                   )
   :effect    (and (at start (not (at ?p ?from)))
                   (at start (not (clear ?to)))
                   (at end (at ?p ?to))
                   (at end (clear ?from))
                   )
   )

  (:durative-action push-to-nongoal
   :parameters (?p - player ?s - stone
                ?ppos - location ?from - location ?to - location
                ?dir - direction)
   :duration (= ?duration 1)
   :condition (and (at start (at ?p ?ppos))
                   (at start (at ?s ?from))
                   (at start (clear ?to))
                   (over all (MOVE-DIR ?ppos ?from ?dir))
                   (over all (MOVE-DIR ?from ?to ?dir))
                   (over all (IS-NONGOAL ?to))
                   )
   :effect    (and (at start (not (at ?p ?ppos)))
                   (at start (not (at ?s ?from)))
                   (at start (not (clear ?to)))
                   (at end (at ?p ?from))
                   (at end (at ?s ?to))
                   (at end (clear ?ppos))
                   (at start (not (at-goal ?s)))
                   )
   )

  (:durative-action push-to-goal
   :parameters (?p - player ?s - stone
                ?ppos - location ?from - location ?to - location
                ?dir - direction)
   :duration  (= ?duration 1)
   :condition (and (at start (at ?p ?ppos))
                   (at start (at ?s ?from))
                   (at start (clear ?to))
                   (over all (MOVE-DIR ?ppos ?from ?dir))
                   (over all (MOVE-DIR ?from ?to ?dir))
                   (over all (IS-GOAL ?to))
                   )
   :effect    (and (at start (not (at ?p ?ppos)))
                   (at start (not (at ?s ?from)))
                   (at start (not (clear ?to)))
                   (at end (at ?p ?from))
                   (at end (at ?s ?to))
                   (at end (clear ?ppos))
                   (at end (at-goal ?s))
                   )
   )
)
