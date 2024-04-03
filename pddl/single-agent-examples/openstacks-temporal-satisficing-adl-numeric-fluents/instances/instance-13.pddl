(define (problem os-time-p17_1)
(:domain openstacks-time-numeric-ADL)
(:objects 
o1 o2 o3 o4 o5 o6 o7 o8 o9 o10 o11 o12 o13 o14 o15 o16 o17  - order
p1 p2 p3 p4 p5 p6 p7 p8 p9 p10 p11 p12 p13 p14 p15 p16 p17  - product

)

(:init
(= (stacks-in-use) 0)
(= (max-stacks) 15)

(waiting o1)
(includes o1 p12)(includes o1 p13)

(waiting o2)
(includes o2 p3)(includes o2 p10)

(waiting o3)
(includes o3 p16)

(waiting o4)
(includes o4 p2)

(waiting o5)
(includes o5 p5)(includes o5 p7)(includes o5 p11)

(waiting o6)
(includes o6 p7)

(waiting o7)
(includes o7 p4)(includes o7 p6)

(waiting o8)
(includes o8 p7)(includes o8 p14)

(waiting o9)
(includes o9 p8)(includes o9 p15)

(waiting o10)
(includes o10 p3)

(waiting o11)
(includes o11 p5)(includes o11 p17)

(waiting o12)
(includes o12 p9)

(waiting o13)
(includes o13 p15)

(waiting o14)
(includes o14 p15)

(waiting o15)
(includes o15 p1)(includes o15 p13)

(waiting o16)
(includes o16 p8)

(waiting o17)
(includes o17 p15)

(= (make-time p1) 50)(= (make-time p2) 30)(= (make-time p3) 40)(= (make-time p4) 20)(= (make-time p5) 100)(= (make-time p6) 20)(= (make-time p7) 10)(= (make-time p8) 100)(= (make-time p9) 100)(= (make-time p10) 80)(= (make-time p11) 70)(= (make-time p12) 50)(= (make-time p13) 90)(= (make-time p14) 50)(= (make-time p15) 100)(= (make-time p16) 90)(= (make-time p17) 60)

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
(shipped o15)
(shipped o16)
(shipped o17)
))

(:metric minimize (total-time))

)

