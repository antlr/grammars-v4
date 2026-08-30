(define

 (problem wireless-01)
 (:domain wireless)
 (:objects
  base - base
  node1 node2 node3 node4 node5 - sensor
  msg1-1 - message  ;; ...just like a token in a Petri-net.
 )

 (:init

  ;; CONFIGURATION DATA
  (higher High Low)
  (higher High Normal)
  (higher High Zero)
  (higher Normal Low)
  (higher Normal Zero)
  (higher Low Zero)

  (next High Normal)
  (next  Normal Low)
  (next  Low Zero)

  ;; NODE1 INFO
  (energy node1 Normal)
  (is-message-at msg1-1 node1)

  ;; NODE2 INFO
  (energy node2 Normal)

  ;; NODE3 INFO
  (energy node3 Normal)

  ;; NODE4 INFO
  (energy node4 Normal)

  ;; NODE5 INFO
  (energy node5 Normal)

  ;; NODE RELATIONS
  (neighbor node1 node2)
  (neighbor node2 node1)
  (neighbor node2 node3)
  (neighbor node3 node2)
  (neighbor node3 node4)
  (neighbor node4 node3)
  (neighbor node3 node5)
  (neighbor node5 node3)
  (neighbor node4 node5)
  (neighbor node5 node4)
  (neighbor node5 base)
  (neighbor base node5)

 )

 (:goal
  (and (has-data base node1)
    (has-data base node2)
    (has-data base node3)
    (has-data base node4)
    (has-data base node5)
  )
 )

)
