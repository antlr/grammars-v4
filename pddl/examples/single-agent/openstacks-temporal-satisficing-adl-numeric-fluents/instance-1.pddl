(define (problem os-time-p5_1)
(:domain openstacks-time-numeric-ADL)
(:objects 
o1 o2 o3 o4 o5  - order
p1 p2 p3 p4 p5  - product

)

(:init
(= (stacks-in-use) 0)
(= (max-stacks) 4)

(waiting o1)
(includes o1 p2)

(waiting o2)
(includes o2 p1)(includes o2 p2)

(waiting o3)
(includes o3 p3)

(waiting o4)
(includes o4 p3)(includes o4 p4)

(waiting o5)
(includes o5 p5)

(= (make-time p1) 40)(= (make-time p2) 50)(= (make-time p3) 80)(= (make-time p4) 40)(= (make-time p5) 10)

)

(:goal
(and
(shipped o1)
(shipped o2)
(shipped o3)
(shipped o4)
(shipped o5)
))

(:metric minimize (total-cost))

)

