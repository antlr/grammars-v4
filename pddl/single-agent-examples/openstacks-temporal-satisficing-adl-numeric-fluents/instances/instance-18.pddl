(define (problem os-time-p22_1)
(:domain openstacks-time-numeric-ADL)
(:objects 
o1 o2 o3 o4 o5 o6 o7 o8 o9 o10 o11 o12 o13 o14 o15 o16 o17 o18 o19 o20 o21 o22  - order
p1 p2 p3 p4 p5 p6 p7 p8 p9 p10 p11 p12 p13 p14 p15 p16 p17 p18 p19 p20 p21 p22  - product

)

(:init
(= (stacks-in-use) 0)
(= (max-stacks) 19)

(waiting o1)
(includes o1 p9)

(waiting o2)
(includes o2 p19)

(waiting o3)
(includes o3 p7)(includes o3 p21)

(waiting o4)
(includes o4 p10)

(waiting o5)
(includes o5 p15)(includes o5 p19)

(waiting o6)
(includes o6 p12)

(waiting o7)
(includes o7 p12)

(waiting o8)
(includes o8 p8)(includes o8 p12)(includes o8 p15)

(waiting o9)
(includes o9 p3)(includes o9 p11)(includes o9 p18)

(waiting o10)
(includes o10 p4)

(waiting o11)
(includes o11 p14)

(waiting o12)
(includes o12 p3)

(waiting o13)
(includes o13 p3)(includes o13 p6)(includes o13 p7)(includes o13 p8)(includes o13 p9)(includes o13 p13)

(waiting o14)
(includes o14 p10)(includes o14 p22)

(waiting o15)
(includes o15 p1)(includes o15 p16)(includes o15 p17)

(waiting o16)
(includes o16 p11)

(waiting o17)
(includes o17 p15)

(waiting o18)
(includes o18 p17)

(waiting o19)
(includes o19 p20)

(waiting o20)
(includes o20 p2)(includes o20 p5)(includes o20 p8)

(waiting o21)
(includes o21 p14)

(waiting o22)
(includes o22 p14)

(= (make-time p1) 30)(= (make-time p2) 60)(= (make-time p3) 90)(= (make-time p4) 60)(= (make-time p5) 80)(= (make-time p6) 40)(= (make-time p7) 90)(= (make-time p8) 40)(= (make-time p9) 30)(= (make-time p10) 100)(= (make-time p11) 100)(= (make-time p12) 60)(= (make-time p13) 100)(= (make-time p14) 90)(= (make-time p15) 70)(= (make-time p16) 40)(= (make-time p17) 80)(= (make-time p18) 80)(= (make-time p19) 10)(= (make-time p20) 10)(= (make-time p21) 70)(= (make-time p22) 10)

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
(shipped o22)
))

(:metric minimize (total-time))

)

