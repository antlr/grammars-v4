(define (problem os-time-p8_1)
(:domain openstacks-time-numeric-ADL)
(:objects 
o1 o2 o3 o4 o5 o6 o7 o8  - order
p1 p2 p3 p4 p5 p6 p7 p8  - product

)

(:init
(= (stacks-in-use) 0)
(= (max-stacks) 7)

(waiting o1)
(includes o1 p1)

(waiting o2)
(includes o2 p5)

(waiting o3)
(includes o3 p4)(includes o3 p8)

(waiting o4)
(includes o4 p7)

(waiting o5)
(includes o5 p2)(includes o5 p6)

(waiting o6)
(includes o6 p1)

(waiting o7)
(includes o7 p1)

(waiting o8)
(includes o8 p3)

(= (make-time p1) 80)(= (make-time p2) 60)(= (make-time p3) 80)(= (make-time p4) 30)(= (make-time p5) 40)(= (make-time p6) 50)(= (make-time p7) 40)(= (make-time p8) 20)

)

(:goal
(and
(shipped o1)
(shipped o2)
(shipped o3)
(shipped o4)
(shipped o5)
(shipped o6)
(shipped o7)
(shipped o8)
))

(:metric minimize (total-time))

)

