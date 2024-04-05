(define

 (problem wireless-10)
 (:domain wireless)
 (:objects
  base - base
  node1 node2 node3 node4 node5 node6 node7 node8 node9 - sensor
  msg1-1 msg2-1 msg3-1 msg4-1 msg5-1 msg6-1 msg7-1 msg8-1 msg9-1 msg10-1 - message  ;; ...just like tokens in a Petri-net.
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
  (energy node1 High)
  (is-message-at msg1-1 node1)

  ;; NODE2 INFO
  (energy node2 High)
  (is-message-at msg2-1 node2)

  ;; NODE3 INFO
  (energy node3 High)
  (is-message-at msg3-1 node3)

  ;; NODE4 INFO
  (energy node4 High)
  (is-message-at msg4-1 node4)

  ;; NODE5 INFO
  (energy node5 High)
  (is-message-at msg5-1 node5)
  
  ;; NODE6 INFO
  (energy node6 High)
  (is-message-at msg6-1 node6)
  
  ;; NODE7 INFO
  (energy node7 High)
  (is-message-at msg7-1 node7)

  ;; NODE8 INFO
  (energy node8 High)
  (is-message-at msg8-1 node8)
  
  ;; NODE9 INFO
  (energy node9 High)
  (is-message-at msg9-1 node9)
    
  ;; NODE RELATIONS
  (neighbor node1 node2)
  (neighbor node1 node9)
  (neighbor node2 node1)
  (neighbor node2 node3)
  (neighbor node2 node9)
  (neighbor node3 node2)
  (neighbor node3 node4)
  (neighbor node4 node3)
  (neighbor node3 node5)
  (neighbor node5 node3)
  (neighbor node4 node5)
  (neighbor node5 node4)
  (neighbor node5 base)
  (neighbor node5 node6)
  (neighbor base node5)
  (neighbor node6 node5)
  (neighbor node6 node7)
  (neighbor node6 node8)
  (neighbor node6 node9)
  (neighbor node7 node6)
  (neighbor node7 node8)
  (neighbor node8 node6)
  (neighbor node8 node7)
  (neighbor node9 node1)
  (neighbor node9 node2)
  (neighbor node9 node6)
  
 )

 (:goal
  (and (has-data base node1)
    (has-data base node2)
    (has-data base node3)
    (has-data base node4)
    (has-data base node5)
    (has-data base node6)
    (has-data base node7)
    (has-data base node8)
    (has-data base node9)
  )
 )

)
