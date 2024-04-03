(define (problem os-time-p25_1)
(:domain openstacks-time-numeric-ADL)
(:objects 
o1 o2 o3 o4 o5 o6 o7 o8 o9 o10 o11 o12 o13 o14 o15 o16 o17 o18 o19 o20 o21 o22 o23 o24 o25  - order
p1 p2 p3 p4 p5 p6 p7 p8 p9 p10 p11 p12 p13 p14 p15 p16 p17 p18 p19 p20 p21 p22 p23 p24 p25  - product

)

(:init
(= (stacks-in-use) 0)
(= (max-stacks) 22)

(waiting o1)
(includes o1 p12)

(waiting o2)
(includes o2 p2)

(waiting o3)
(includes o3 p1)(includes o3 p4)(includes o3 p7)(includes o3 p13)

(waiting o4)
(includes o4 p2)

(waiting o5)
(includes o5 p6)(includes o5 p16)(includes o5 p24)

(waiting o6)
(includes o6 p22)

(waiting o7)
(includes o7 p20)

(waiting o8)
(includes o8 p8)

(waiting o9)
(includes o9 p3)

(waiting o10)
(includes o10 p1)

(waiting o11)
(includes o11 p12)

(waiting o12)
(includes o12 p8)(includes o12 p19)

(waiting o13)
(includes o13 p11)(includes o13 p14)(includes o13 p18)

(waiting o14)
(includes o14 p10)(includes o14 p18)

(waiting o15)
(includes o15 p4)(includes o15 p15)

(waiting o16)
(includes o16 p5)(includes o16 p20)

(waiting o17)
(includes o17 p9)

(waiting o18)
(includes o18 p14)

(waiting o19)
(includes o19 p17)(includes o19 p21)(includes o19 p25)

(waiting o20)
(includes o20 p24)

(waiting o21)
(includes o21 p9)

(waiting o22)
(includes o22 p22)(includes o22 p24)

(waiting o23)
(includes o23 p14)

(waiting o24)
(includes o24 p23)

(waiting o25)
(includes o25 p21)

(= (make-time p1) 80)(= (make-time p2) 10)(= (make-time p3) 40)(= (make-time p4) 10)(= (make-time p5) 80)(= (make-time p6) 100)(= (make-time p7) 90)(= (make-time p8) 90)(= (make-time p9) 50)(= (make-time p10) 30)(= (make-time p11) 70)(= (make-time p12) 60)(= (make-time p13) 20)(= (make-time p14) 100)(= (make-time p15) 20)(= (make-time p16) 70)(= (make-time p17) 70)(= (make-time p18) 100)(= (make-time p19) 20)(= (make-time p20) 10)(= (make-time p21) 80)(= (make-time p22) 90)(= (make-time p23) 100)(= (make-time p24) 10)(= (make-time p25) 20)

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
(shipped o25)
))

(:metric minimize (total-time))

)

