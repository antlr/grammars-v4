(define (problem os-time-p30_1)
(:domain openstacks-time-numeric-ADL)
(:objects 
o1 o2 o3 o4 o5 o6 o7 o8 o9 o10 o11 o12 o13 o14 o15 o16 o17 o18 o19 o20 o21 o22 o23 o24 o25 o26 o27 o28 o29 o30  - order
p1 p2 p3 p4 p5 p6 p7 p8 p9 p10 p11 p12 p13 p14 p15 p16 p17 p18 p19 p20 p21 p22 p23 p24 p25 p26 p27 p28 p29 p30  - product

)

(:init
(= (stacks-in-use) 0)
(= (max-stacks) 27)

(waiting o1)
(includes o1 p11)

(waiting o2)
(includes o2 p22)

(waiting o3)
(includes o3 p13)

(waiting o4)
(includes o4 p10)(includes o4 p13)(includes o4 p16)

(waiting o5)
(includes o5 p1)(includes o5 p5)(includes o5 p6)

(waiting o6)
(includes o6 p4)(includes o6 p12)

(waiting o7)
(includes o7 p23)

(waiting o8)
(includes o8 p8)(includes o8 p9)

(waiting o9)
(includes o9 p6)

(waiting o10)
(includes o10 p18)

(waiting o11)
(includes o11 p24)

(waiting o12)
(includes o12 p22)

(waiting o13)
(includes o13 p7)

(waiting o14)
(includes o14 p6)

(waiting o15)
(includes o15 p13)(includes o15 p18)(includes o15 p30)

(waiting o16)
(includes o16 p13)

(waiting o17)
(includes o17 p14)

(waiting o18)
(includes o18 p25)(includes o18 p30)

(waiting o19)
(includes o19 p18)

(waiting o20)
(includes o20 p6)(includes o20 p7)(includes o20 p29)

(waiting o21)
(includes o21 p15)(includes o21 p19)

(waiting o22)
(includes o22 p2)(includes o22 p20)

(waiting o23)
(includes o23 p3)(includes o23 p16)

(waiting o24)
(includes o24 p13)

(waiting o25)
(includes o25 p7)

(waiting o26)
(includes o26 p20)(includes o26 p23)(includes o26 p28)

(waiting o27)
(includes o27 p17)(includes o27 p21)(includes o27 p26)(includes o27 p27)

(waiting o28)
(includes o28 p11)(includes o28 p12)

(waiting o29)
(includes o29 p15)

(waiting o30)
(includes o30 p21)

(= (make-time p1) 20)(= (make-time p2) 50)(= (make-time p3) 50)(= (make-time p4) 80)(= (make-time p5) 10)(= (make-time p6) 60)(= (make-time p7) 90)(= (make-time p8) 90)(= (make-time p9) 40)(= (make-time p10) 100)(= (make-time p11) 20)(= (make-time p12) 50)(= (make-time p13) 80)(= (make-time p14) 100)(= (make-time p15) 90)(= (make-time p16) 10)(= (make-time p17) 20)(= (make-time p18) 30)(= (make-time p19) 60)(= (make-time p20) 20)(= (make-time p21) 70)(= (make-time p22) 30)(= (make-time p23) 90)(= (make-time p24) 10)(= (make-time p25) 40)(= (make-time p26) 70)(= (make-time p27) 30)(= (make-time p28) 60)(= (make-time p29) 40)(= (make-time p30) 40)

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
(shipped o29)
(shipped o30)
))

(:metric minimize (total-time))

)

