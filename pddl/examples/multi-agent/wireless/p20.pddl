(define

 (problem wireless-20)
 (:domain wireless)
 (:objects
  base - base
  node1 node2 node3 node5 node6 node7 node8 node9 node11 - sensor
  msg1-1 msg2-1 msg3-1 msg4-1 msg5-1 msg6-1 msg7-1 msg8-1 msg9-1 msg10-1 msg11-1 msg12-1 msg1-2 msg2-2 msg3-2 msg4-2 msg5-2 msg6-2 msg7-2 msg8-2 msg9-2 msg10-2 msg11-2 msg12-2 - message  ;; ...just like tokens in a Petri-net.
  Very-High - level
 )

 (:init

  ;; CONFIGURATION DATA
  (higher Very-High High)
  (higher Very-High Low)
  (higher Very-High Normal)
  (higher Very-High Zero)
  (higher High  Low)
  (higher High  Normal)
  (higher High  Zero)
  (higher Normal  Low)
  (higher Normal  Zero)
  (higher Low   Zero)

  (next Very-High High)
  (next High   Normal)
  (next  Normal  Low)
  (next  Low   Zero)

  ;; NODE1 INFO
  (energy node1 Very-High)
  (is-message-at msg1-1 node1)
  (is-message-at msg1-2 node1)

  ;; NODE2 INFO
  (energy node2 Very-High)
  (is-message-at msg2-1 node2)
  (is-message-at msg2-2 node2)
  
  ;; NODE3 INFO
  (energy node3 Very-High)
  (is-message-at msg3-1 node3)
  (is-message-at msg3-2 node3)
    
  ;; NODE5 INFO
  (energy node5 Very-High)
  (is-message-at msg5-1 node5)
  (is-message-at msg5-2 node5)

  ;; NODE6 INFO
  (energy node6 Very-High)
  (is-message-at msg6-1 node6)
  (is-message-at msg6-2 node6)
  
  ;; NODE7 INFO
  (energy node7 Very-High)
  (is-message-at msg7-1 node7)
  (is-message-at msg7-2 node7)
  
  ;; NODE8 INFO
  (energy node8 Very-High)
  (is-message-at msg8-1 node8)
  (is-message-at msg8-2 node8)
  
  ;; NODE9 INFO
  (energy node9 Very-High)
  (is-message-at msg9-1 node9)
  (is-message-at msg9-2 node9)
    
  ;; NODE11 INFO
  (energy node11 Very-High)
  (is-message-at msg11-1 node11)
  (is-message-at msg11-2 node11)
  
  ;; NODE RELATIONS
  (neighbor node1 node2)
  (neighbor node1 node9)
  (neighbor node2 node1)
  (neighbor node2 node3)
  (neighbor node2 node6)
  (neighbor node2 node9)
  (neighbor node3 node2)
  (neighbor node3 node5)
  (neighbor node3 node6)
  (neighbor node5 node3)
  (neighbor node5 base)
  (neighbor node5 node6)
  (neighbor base node5)
  (neighbor base node6)
  (neighbor base node7)
  (neighbor base node11)
  (neighbor node6 node2)
  (neighbor node6 node3)
  (neighbor node6 node5)
  (neighbor node6 base)
  (neighbor node6 node7)
  (neighbor node6 node8)
  (neighbor node6 node9)
  (neighbor node7 base)
  (neighbor node7 node6)
  (neighbor node7 node8)
  (neighbor node7 node11)
  (neighbor node8 node6)
  (neighbor node8 node7)
  (neighbor node9 node1)
  (neighbor node9 node2)
  (neighbor node9 node6)
  (neighbor node11 base)
  (neighbor node11 node7)
 )

 (:goal
  (and (has-data base node1)
    (has-data base node2)
    (has-data base node3)
    (has-data base node5)
    (has-data base node6)
    (has-data base node7)
    (has-data base node8)
    (has-data base node9)
    (has-data base node11)
  )
 )

)
