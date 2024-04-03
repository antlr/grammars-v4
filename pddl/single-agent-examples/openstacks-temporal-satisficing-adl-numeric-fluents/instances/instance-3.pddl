(define (problem os-time-p7_1)
(:domain openstacks-time-numeric-ADL)
(:objects 
o1 o2 o3 o4 o5 o6 o7  - order
p1 p2 p3 p4 p5 p6 p7  - product

)

(:init
(= (stacks-in-use) 0)
(= (max-stacks) 6)

(waiting o1)
(includes o1 p2)

(waiting o2)
(includes o2 p2)

(waiting o3)
(includes o3 p3)

(waiting o4)
(includes o4 p4)

(waiting o5)
(includes o5 p5)(includes o5 p7)

(waiting o6)
(includes o6 p1)

(waiting o7)
(includes o7 p6)

(= (make-time p1) 10)(= (make-time p2) 80)(= (make-time p3) 60)(= (make-time p4) 10)(= (make-time p5) 10)(= (make-time p6) 80)(= (make-time p7) 60)

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
))

(:metric minimize (total-time))

)

