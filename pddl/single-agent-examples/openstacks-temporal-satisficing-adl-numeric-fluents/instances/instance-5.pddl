(define (problem os-time-p9_1)
(:domain openstacks-time-numeric-ADL)
(:objects 
o1 o2 o3 o4 o5 o6 o7 o8 o9  - order
p1 p2 p3 p4 p5 p6 p7 p8 p9  - product

)

(:init
(= (stacks-in-use) 0)
(= (max-stacks) 8)

(waiting o1)
(includes o1 p5)

(waiting o2)
(includes o2 p4)(includes o2 p8)

(waiting o3)
(includes o3 p4)(includes o3 p6)

(waiting o4)
(includes o4 p4)

(waiting o5)
(includes o5 p7)

(waiting o6)
(includes o6 p1)(includes o6 p2)(includes o6 p9)

(waiting o7)
(includes o7 p5)

(waiting o8)
(includes o8 p3)(includes o8 p4)

(waiting o9)
(includes o9 p5)

(= (make-time p1) 80)(= (make-time p2) 70)(= (make-time p3) 80)(= (make-time p4) 70)(= (make-time p5) 20)(= (make-time p6) 70)(= (make-time p7) 60)(= (make-time p8) 50)(= (make-time p9) 20)

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
(shipped o9)
))

(:metric minimize (total-time))

)

