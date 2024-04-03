(define (problem os-time-p27_1)
(:domain openstacks-time-numeric-ADL)
(:objects 
o1 o2 o3 o4 o5 o6 o7 o8 o9 o10 o11 o12 o13 o14 o15 o16 o17 o18 o19 o20 o21 o22 o23 o24 o25 o26 o27  - order
p1 p2 p3 p4 p5 p6 p7 p8 p9 p10 p11 p12 p13 p14 p15 p16 p17 p18 p19 p20 p21 p22 p23 p24 p25 p26 p27  - product

)

(:init
(= (stacks-in-use) 0)
(= (max-stacks) 24)

(waiting o1)
(includes o1 p15)(includes o1 p19)(includes o1 p27)

(waiting o2)
(includes o2 p11)

(waiting o3)
(includes o3 p5)

(waiting o4)
(includes o4 p4)(includes o4 p13)

(waiting o5)
(includes o5 p23)

(waiting o6)
(includes o6 p24)

(waiting o7)
(includes o7 p9)(includes o7 p10)(includes o7 p13)

(waiting o8)
(includes o8 p3)(includes o8 p20)(includes o8 p26)

(waiting o9)
(includes o9 p20)

(waiting o10)
(includes o10 p13)(includes o10 p21)

(waiting o11)
(includes o11 p14)

(waiting o12)
(includes o12 p5)

(waiting o13)
(includes o13 p19)

(waiting o14)
(includes o14 p1)

(waiting o15)
(includes o15 p14)

(waiting o16)
(includes o16 p6)(includes o16 p7)(includes o16 p10)

(waiting o17)
(includes o17 p12)

(waiting o18)
(includes o18 p20)(includes o18 p22)

(waiting o19)
(includes o19 p16)(includes o19 p25)

(waiting o20)
(includes o20 p2)(includes o20 p17)(includes o20 p26)

(waiting o21)
(includes o21 p19)

(waiting o22)
(includes o22 p27)

(waiting o23)
(includes o23 p20)

(waiting o24)
(includes o24 p6)(includes o24 p8)(includes o24 p10)(includes o24 p18)

(waiting o25)
(includes o25 p14)

(waiting o26)
(includes o26 p6)(includes o26 p7)(includes o26 p10)

(waiting o27)
(includes o27 p19)(includes o27 p23)

(= (make-time p1) 50)(= (make-time p2) 80)(= (make-time p3) 80)(= (make-time p4) 10)(= (make-time p5) 10)(= (make-time p6) 50)(= (make-time p7) 80)(= (make-time p8) 80)(= (make-time p9) 50)(= (make-time p10) 10)(= (make-time p11) 90)(= (make-time p12) 90)(= (make-time p13) 90)(= (make-time p14) 20)(= (make-time p15) 10)(= (make-time p16) 20)(= (make-time p17) 80)(= (make-time p18) 40)(= (make-time p19) 50)(= (make-time p20) 60)(= (make-time p21) 80)(= (make-time p22) 100)(= (make-time p23) 80)(= (make-time p24) 100)(= (make-time p25) 40)(= (make-time p26) 50)(= (make-time p27) 40)

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
(shipped o26)
(shipped o27)
))

(:metric minimize (total-time))

)

