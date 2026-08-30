(define (problem os-time-p24_1)
(:domain openstacks-time-numeric-ADL)
(:objects 
o1 o2 o3 o4 o5 o6 o7 o8 o9 o10 o11 o12 o13 o14 o15 o16 o17 o18 o19 o20 o21 o22 o23 o24  - order
p1 p2 p3 p4 p5 p6 p7 p8 p9 p10 p11 p12 p13 p14 p15 p16 p17 p18 p19 p20 p21 p22 p23 p24  - product

)

(:init
(= (stacks-in-use) 0)
(= (max-stacks) 21)

(waiting o1)
(includes o1 p10)(includes o1 p13)

(waiting o2)
(includes o2 p6)(includes o2 p7)

(waiting o3)
(includes o3 p21)

(waiting o4)
(includes o4 p13)(includes o4 p14)

(waiting o5)
(includes o5 p4)

(waiting o6)
(includes o6 p2)(includes o6 p3)

(waiting o7)
(includes o7 p9)(includes o7 p18)(includes o7 p21)

(waiting o8)
(includes o8 p1)

(waiting o9)
(includes o9 p12)(includes o9 p15)(includes o9 p18)

(waiting o10)
(includes o10 p11)

(waiting o11)
(includes o11 p12)

(waiting o12)
(includes o12 p5)

(waiting o13)
(includes o13 p19)(includes o13 p21)(includes o13 p24)

(waiting o14)
(includes o14 p23)

(waiting o15)
(includes o15 p16)

(waiting o16)
(includes o16 p4)(includes o16 p22)

(waiting o17)
(includes o17 p20)

(waiting o18)
(includes o18 p17)(includes o18 p19)

(waiting o19)
(includes o19 p13)(includes o19 p18)

(waiting o20)
(includes o20 p10)

(waiting o21)
(includes o21 p1)

(waiting o22)
(includes o22 p8)(includes o22 p12)(includes o22 p14)

(waiting o23)
(includes o23 p16)

(waiting o24)
(includes o24 p19)

(= (make-time p1) 40)(= (make-time p2) 30)(= (make-time p3) 20)(= (make-time p4) 10)(= (make-time p5) 70)(= (make-time p6) 70)(= (make-time p7) 10)(= (make-time p8) 50)(= (make-time p9) 60)(= (make-time p10) 40)(= (make-time p11) 100)(= (make-time p12) 100)(= (make-time p13) 30)(= (make-time p14) 70)(= (make-time p15) 40)(= (make-time p16) 50)(= (make-time p17) 80)(= (make-time p18) 10)(= (make-time p19) 80)(= (make-time p20) 60)(= (make-time p21) 50)(= (make-time p22) 30)(= (make-time p23) 100)(= (make-time p24) 90)

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
(shipped o23)
(shipped o24)
))

(:metric minimize (total-cost))

)

