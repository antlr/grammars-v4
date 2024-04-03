(define (problem os-time-p13_1)
(:domain openstacks-time-numeric-ADL)
(:objects 
o1 o2 o3 o4 o5 o6 o7 o8 o9 o10 o11 o12 o13  - order
p1 p2 p3 p4 p5 p6 p7 p8 p9 p10 p11 p12 p13  - product

)

(:init
(= (stacks-in-use) 0)
(= (max-stacks) 11)

(waiting o1)
(includes o1 p10)

(waiting o2)
(includes o2 p7)

(waiting o3)
(includes o3 p7)

(waiting o4)
(includes o4 p5)(includes o4 p9)

(waiting o5)
(includes o5 p7)

(waiting o6)
(includes o6 p9)(includes o6 p11)

(waiting o7)
(includes o7 p2)

(waiting o8)
(includes o8 p4)

(waiting o9)
(includes o9 p10)

(waiting o10)
(includes o10 p4)(includes o10 p13)

(waiting o11)
(includes o11 p10)

(waiting o12)
(includes o12 p1)(includes o12 p3)(includes o12 p4)(includes o12 p6)(includes o12 p8)

(waiting o13)
(includes o13 p12)

(= (make-time p1) 90)(= (make-time p2) 20)(= (make-time p3) 70)(= (make-time p4) 30)(= (make-time p5) 30)(= (make-time p6) 20)(= (make-time p7) 70)(= (make-time p8) 30)(= (make-time p9) 70)(= (make-time p10) 60)(= (make-time p11) 90)(= (make-time p12) 50)(= (make-time p13) 80)

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
(shipped o12)
(shipped o13)
))

(:metric minimize (total-time))

)

