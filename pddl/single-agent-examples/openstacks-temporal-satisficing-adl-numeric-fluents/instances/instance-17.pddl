(define (problem os-time-p21_1)
(:domain openstacks-time-numeric-ADL)
(:objects 
o1 o2 o3 o4 o5 o6 o7 o8 o9 o10 o11 o12 o13 o14 o15 o16 o17 o18 o19 o20 o21  - order
p1 p2 p3 p4 p5 p6 p7 p8 p9 p10 p11 p12 p13 p14 p15 p16 p17 p18 p19 p20 p21  - product

)

(:init
(= (stacks-in-use) 0)
(= (max-stacks) 18)

(waiting o1)
(includes o1 p1)(includes o1 p21)

(waiting o2)
(includes o2 p7)

(waiting o3)
(includes o3 p3)

(waiting o4)
(includes o4 p18)

(waiting o5)
(includes o5 p5)(includes o5 p9)(includes o5 p19)

(waiting o6)
(includes o6 p17)

(waiting o7)
(includes o7 p12)(includes o7 p15)

(waiting o8)
(includes o8 p6)(includes o8 p13)

(waiting o9)
(includes o9 p2)(includes o9 p12)(includes o9 p16)

(waiting o10)
(includes o10 p5)(includes o10 p9)

(waiting o11)
(includes o11 p8)(includes o11 p14)

(waiting o12)
(includes o12 p11)(includes o12 p12)

(waiting o13)
(includes o13 p17)

(waiting o14)
(includes o14 p12)

(waiting o15)
(includes o15 p8)(includes o15 p9)

(waiting o16)
(includes o16 p17)(includes o16 p18)(includes o16 p20)

(waiting o17)
(includes o17 p10)

(waiting o18)
(includes o18 p8)

(waiting o19)
(includes o19 p4)(includes o19 p17)

(waiting o20)
(includes o20 p18)

(waiting o21)
(includes o21 p13)

(= (make-time p1) 100)(= (make-time p2) 90)(= (make-time p3) 90)(= (make-time p4) 90)(= (make-time p5) 50)(= (make-time p6) 100)(= (make-time p7) 20)(= (make-time p8) 90)(= (make-time p9) 20)(= (make-time p10) 20)(= (make-time p11) 30)(= (make-time p12) 70)(= (make-time p13) 10)(= (make-time p14) 90)(= (make-time p15) 40)(= (make-time p16) 10)(= (make-time p17) 40)(= (make-time p18) 70)(= (make-time p19) 10)(= (make-time p20) 50)(= (make-time p21) 30)

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
(shipped o21)
))

(:metric minimize (total-time))

)

