(define (problem os-time-p14_1)
(:domain openstacks-time-numeric-ADL)
(:objects 
o1 o2 o3 o4 o5 o6 o7 o8 o9 o10 o11 o12 o13 o14  - order
p1 p2 p3 p4 p5 p6 p7 p8 p9 p10 p11 p12 p13 p14  - product

)

(:init
(= (stacks-in-use) 0)
(= (max-stacks) 12)

(waiting o1)
(includes o1 p14)

(waiting o2)
(includes o2 p6)

(waiting o3)
(includes o3 p3)(includes o3 p10)

(waiting o4)
(includes o4 p3)

(waiting o5)
(includes o5 p7)

(waiting o6)
(includes o6 p9)

(waiting o7)
(includes o7 p3)(includes o7 p6)(includes o7 p7)(includes o7 p11)

(waiting o8)
(includes o8 p2)

(waiting o9)
(includes o9 p7)

(waiting o10)
(includes o10 p5)(includes o10 p11)

(waiting o11)
(includes o11 p8)

(waiting o12)
(includes o12 p4)(includes o12 p6)

(waiting o13)
(includes o13 p8)(includes o13 p9)(includes o13 p12)(includes o13 p13)

(waiting o14)
(includes o14 p1)

(= (make-time p1) 70)(= (make-time p2) 10)(= (make-time p3) 40)(= (make-time p4) 100)(= (make-time p5) 100)(= (make-time p6) 50)(= (make-time p7) 40)(= (make-time p8) 60)(= (make-time p9) 10)(= (make-time p10) 30)(= (make-time p11) 60)(= (make-time p12) 40)(= (make-time p13) 100)(= (make-time p14) 50)

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
(shipped o14)
))

(:metric minimize (total-cost))

)

