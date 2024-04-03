(define (problem os-time-p20_1)
(:domain openstacks-time-numeric-ADL)
(:objects 
o1 o2 o3 o4 o5 o6 o7 o8 o9 o10 o11 o12 o13 o14 o15 o16 o17 o18 o19 o20  - order
p1 p2 p3 p4 p5 p6 p7 p8 p9 p10 p11 p12 p13 p14 p15 p16 p17 p18 p19 p20  - product

)

(:init
(= (stacks-in-use) 0)
(= (max-stacks) 18)

(waiting o1)
(includes o1 p2)

(waiting o2)
(includes o2 p3)(includes o2 p4)(includes o2 p5)(includes o2 p7)

(waiting o3)
(includes o3 p5)(includes o3 p6)(includes o3 p8)

(waiting o4)
(includes o4 p16)

(waiting o5)
(includes o5 p10)

(waiting o6)
(includes o6 p16)

(waiting o7)
(includes o7 p12)

(waiting o8)
(includes o8 p4)(includes o8 p12)(includes o8 p13)(includes o8 p17)

(waiting o9)
(includes o9 p4)

(waiting o10)
(includes o10 p4)(includes o10 p10)

(waiting o11)
(includes o11 p12)

(waiting o12)
(includes o12 p3)

(waiting o13)
(includes o13 p6)

(waiting o14)
(includes o14 p18)

(waiting o15)
(includes o15 p12)(includes o15 p14)(includes o15 p16)(includes o15 p18)

(waiting o16)
(includes o16 p11)(includes o16 p20)

(waiting o17)
(includes o17 p1)

(waiting o18)
(includes o18 p9)

(waiting o19)
(includes o19 p15)

(waiting o20)
(includes o20 p15)(includes o20 p19)

(= (make-time p1) 40)(= (make-time p2) 10)(= (make-time p3) 70)(= (make-time p4) 10)(= (make-time p5) 80)(= (make-time p6) 30)(= (make-time p7) 80)(= (make-time p8) 80)(= (make-time p9) 50)(= (make-time p10) 80)(= (make-time p11) 40)(= (make-time p12) 20)(= (make-time p13) 50)(= (make-time p14) 50)(= (make-time p15) 20)(= (make-time p16) 90)(= (make-time p17) 50)(= (make-time p18) 30)(= (make-time p19) 30)(= (make-time p20) 80)

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
(shipped o19)
(shipped o20)
))

(:metric minimize (total-time))

)

