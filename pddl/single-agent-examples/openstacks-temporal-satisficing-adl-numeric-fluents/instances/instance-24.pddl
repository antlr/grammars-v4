(define (problem os-time-p28_1)
(:domain openstacks-time-numeric-ADL)
(:objects 
o1 o2 o3 o4 o5 o6 o7 o8 o9 o10 o11 o12 o13 o14 o15 o16 o17 o18 o19 o20 o21 o22 o23 o24 o25 o26 o27 o28  - order
p1 p2 p3 p4 p5 p6 p7 p8 p9 p10 p11 p12 p13 p14 p15 p16 p17 p18 p19 p20 p21 p22 p23 p24 p25 p26 p27 p28  - product

)

(:init
(= (stacks-in-use) 0)
(= (max-stacks) 25)

(waiting o1)
(includes o1 p19)(includes o1 p24)

(waiting o2)
(includes o2 p14)

(waiting o3)
(includes o3 p17)(includes o3 p22)(includes o3 p26)

(waiting o4)
(includes o4 p12)

(waiting o5)
(includes o5 p4)(includes o5 p23)

(waiting o6)
(includes o6 p2)

(waiting o7)
(includes o7 p1)

(waiting o8)
(includes o8 p7)(includes o8 p15)

(waiting o9)
(includes o9 p1)(includes o9 p6)

(waiting o10)
(includes o10 p3)

(waiting o11)
(includes o11 p13)

(waiting o12)
(includes o12 p8)(includes o12 p12)

(waiting o13)
(includes o13 p8)(includes o13 p15)

(waiting o14)
(includes o14 p12)(includes o14 p21)(includes o14 p22)

(waiting o15)
(includes o15 p13)(includes o15 p25)

(waiting o16)
(includes o16 p8)

(waiting o17)
(includes o17 p16)

(waiting o18)
(includes o18 p10)(includes o18 p20)

(waiting o19)
(includes o19 p3)

(waiting o20)
(includes o20 p11)

(waiting o21)
(includes o21 p21)

(waiting o22)
(includes o22 p11)(includes o22 p12)(includes o22 p16)

(waiting o23)
(includes o23 p22)

(waiting o24)
(includes o24 p11)(includes o24 p12)(includes o24 p19)(includes o24 p28)

(waiting o25)
(includes o25 p6)(includes o25 p9)

(waiting o26)
(includes o26 p18)(includes o26 p27)

(waiting o27)
(includes o27 p5)

(waiting o28)
(includes o28 p15)

(= (make-time p1) 20)(= (make-time p2) 100)(= (make-time p3) 20)(= (make-time p4) 70)(= (make-time p5) 100)(= (make-time p6) 90)(= (make-time p7) 30)(= (make-time p8) 50)(= (make-time p9) 30)(= (make-time p10) 30)(= (make-time p11) 60)(= (make-time p12) 70)(= (make-time p13) 60)(= (make-time p14) 80)(= (make-time p15) 100)(= (make-time p16) 80)(= (make-time p17) 100)(= (make-time p18) 70)(= (make-time p19) 30)(= (make-time p20) 40)(= (make-time p21) 90)(= (make-time p22) 90)(= (make-time p23) 30)(= (make-time p24) 80)(= (make-time p25) 20)(= (make-time p26) 20)(= (make-time p27) 60)(= (make-time p28) 70)

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
(shipped o28)
))

(:metric minimize (total-time))

)

