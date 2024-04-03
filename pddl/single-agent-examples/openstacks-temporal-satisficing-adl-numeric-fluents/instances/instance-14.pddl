(define (problem os-time-p18_1)
(:domain openstacks-time-numeric-ADL)
(:objects 
o1 o2 o3 o4 o5 o6 o7 o8 o9 o10 o11 o12 o13 o14 o15 o16 o17 o18  - order
p1 p2 p3 p4 p5 p6 p7 p8 p9 p10 p11 p12 p13 p14 p15 p16 p17 p18  - product

)

(:init
(= (stacks-in-use) 0)
(= (max-stacks) 16)

(waiting o1)
(includes o1 p4)(includes o1 p6)(includes o1 p11)

(waiting o2)
(includes o2 p8)

(waiting o3)
(includes o3 p13)(includes o3 p17)

(waiting o4)
(includes o4 p3)

(waiting o5)
(includes o5 p2)(includes o5 p13)

(waiting o6)
(includes o6 p3)(includes o6 p9)

(waiting o7)
(includes o7 p17)

(waiting o8)
(includes o8 p4)

(waiting o9)
(includes o9 p15)

(waiting o10)
(includes o10 p17)

(waiting o11)
(includes o11 p12)(includes o11 p13)(includes o11 p16)

(waiting o12)
(includes o12 p1)(includes o12 p13)

(waiting o13)
(includes o13 p5)(includes o13 p6)

(waiting o14)
(includes o14 p8)(includes o14 p10)

(waiting o15)
(includes o15 p14)

(waiting o16)
(includes o16 p14)

(waiting o17)
(includes o17 p5)(includes o17 p7)

(waiting o18)
(includes o18 p8)(includes o18 p18)

(= (make-time p1) 30)(= (make-time p2) 80)(= (make-time p3) 30)(= (make-time p4) 50)(= (make-time p5) 90)(= (make-time p6) 30)(= (make-time p7) 20)(= (make-time p8) 70)(= (make-time p9) 20)(= (make-time p10) 20)(= (make-time p11) 50)(= (make-time p12) 50)(= (make-time p13) 10)(= (make-time p14) 10)(= (make-time p15) 30)(= (make-time p16) 100)(= (make-time p17) 60)(= (make-time p18) 90)

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
(shipped o18)
))

(:metric minimize (total-time))

)

