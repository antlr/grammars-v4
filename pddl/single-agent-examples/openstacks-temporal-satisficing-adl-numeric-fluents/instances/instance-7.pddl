(define (problem os-time-p11_1)
(:domain openstacks-time-numeric-ADL)
(:objects 
o1 o2 o3 o4 o5 o6 o7 o8 o9 o10 o11  - order
p1 p2 p3 p4 p5 p6 p7 p8 p9 p10 p11  - product

)

(:init
(= (stacks-in-use) 0)
(= (max-stacks) 9)

(waiting o1)
(includes o1 p1)

(waiting o2)
(includes o2 p1)(includes o2 p3)

(waiting o3)
(includes o3 p2)(includes o3 p8)

(waiting o4)
(includes o4 p9)(includes o4 p11)

(waiting o5)
(includes o5 p10)

(waiting o6)
(includes o6 p5)(includes o6 p7)

(waiting o7)
(includes o7 p1)(includes o7 p4)(includes o7 p6)

(waiting o8)
(includes o8 p1)

(waiting o9)
(includes o9 p1)

(waiting o10)
(includes o10 p7)

(waiting o11)
(includes o11 p8)

(= (make-time p1) 40)(= (make-time p2) 30)(= (make-time p3) 60)(= (make-time p4) 40)(= (make-time p5) 100)(= (make-time p6) 90)(= (make-time p7) 80)(= (make-time p8) 30)(= (make-time p9) 70)(= (make-time p10) 20)(= (make-time p11) 30)

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
(shipped o11)
))

(:metric minimize (total-time))

)

