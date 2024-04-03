(define (problem os-time-p10_1)
(:domain openstacks-time-numeric-ADL)
(:objects 
o1 o2 o3 o4 o5 o6 o7 o8 o9 o10  - order
p1 p2 p3 p4 p5 p6 p7 p8 p9 p10  - product

)

(:init
(= (stacks-in-use) 0)
(= (max-stacks) 9)

(waiting o1)
(includes o1 p6)(includes o1 p9)(includes o1 p10)

(waiting o2)
(includes o2 p2)(includes o2 p10)

(waiting o3)
(includes o3 p1)(includes o3 p7)(includes o3 p8)

(waiting o4)
(includes o4 p4)

(waiting o5)
(includes o5 p3)

(waiting o6)
(includes o6 p5)

(waiting o7)
(includes o7 p5)

(waiting o8)
(includes o8 p3)

(waiting o9)
(includes o9 p1)

(waiting o10)
(includes o10 p9)

(= (make-time p1) 90)(= (make-time p2) 40)(= (make-time p3) 60)(= (make-time p4) 30)(= (make-time p5) 40)(= (make-time p6) 10)(= (make-time p7) 70)(= (make-time p8) 50)(= (make-time p9) 100)(= (make-time p10) 80)

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
(shipped o10)
))

(:metric minimize (total-time))

)

