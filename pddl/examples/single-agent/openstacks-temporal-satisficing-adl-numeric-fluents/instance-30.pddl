(define (problem os-time-p34_1)
(:domain openstacks-time-numeric-ADL)
(:objects 
o1 o2 o3 o4 o5 o6 o7 o8 o9 o10 o11 o12 o13 o14 o15 o16 o17 o18 o19 o20 o21 o22 o23 o24 o25 o26 o27 o28 o29 o30 o31 o32 o33 o34  - order
p1 p2 p3 p4 p5 p6 p7 p8 p9 p10 p11 p12 p13 p14 p15 p16 p17 p18 p19 p20 p21 p22 p23 p24 p25 p26 p27 p28 p29 p30 p31 p32 p33 p34  - product

)

(:init
(= (stacks-in-use) 0)
(= (max-stacks) 30)

(waiting o1)
(includes o1 p16)

(waiting o2)
(includes o2 p3)

(waiting o3)
(includes o3 p17)(includes o3 p31)

(waiting o4)
(includes o4 p2)

(waiting o5)
(includes o5 p14)(includes o5 p23)

(waiting o6)
(includes o6 p4)(includes o6 p8)

(waiting o7)
(includes o7 p24)

(waiting o8)
(includes o8 p21)(includes o8 p26)(includes o8 p28)

(waiting o9)
(includes o9 p26)

(waiting o10)
(includes o10 p13)(includes o10 p17)(includes o10 p22)

(waiting o11)
(includes o11 p11)

(waiting o12)
(includes o12 p30)

(waiting o13)
(includes o13 p27)

(waiting o14)
(includes o14 p12)

(waiting o15)
(includes o15 p3)(includes o15 p6)(includes o15 p11)

(waiting o16)
(includes o16 p34)

(waiting o17)
(includes o17 p5)(includes o17 p16)(includes o17 p25)

(waiting o18)
(includes o18 p1)(includes o18 p2)

(waiting o19)
(includes o19 p10)

(waiting o20)
(includes o20 p29)

(waiting o21)
(includes o21 p30)

(waiting o22)
(includes o22 p5)

(waiting o23)
(includes o23 p18)(includes o23 p33)

(waiting o24)
(includes o24 p30)

(waiting o25)
(includes o25 p9)(includes o25 p32)

(waiting o26)
(includes o26 p26)

(waiting o27)
(includes o27 p2)

(waiting o28)
(includes o28 p19)(includes o28 p25)

(waiting o29)
(includes o29 p17)(includes o29 p20)(includes o29 p24)(includes o29 p29)

(waiting o30)
(includes o30 p5)(includes o30 p8)(includes o30 p9)(includes o30 p12)

(waiting o31)
(includes o31 p9)(includes o31 p15)

(waiting o32)
(includes o32 p26)(includes o32 p27)

(waiting o33)
(includes o33 p7)(includes o33 p27)

(waiting o34)
(includes o34 p16)

(= (make-time p1) 40)(= (make-time p2) 60)(= (make-time p3) 30)(= (make-time p4) 100)(= (make-time p5) 100)(= (make-time p6) 60)(= (make-time p7) 100)(= (make-time p8) 60)(= (make-time p9) 60)(= (make-time p10) 60)(= (make-time p11) 10)(= (make-time p12) 70)(= (make-time p13) 70)(= (make-time p14) 10)(= (make-time p15) 20)(= (make-time p16) 30)(= (make-time p17) 70)(= (make-time p18) 20)(= (make-time p19) 70)(= (make-time p20) 60)(= (make-time p21) 60)(= (make-time p22) 30)(= (make-time p23) 60)(= (make-time p24) 40)(= (make-time p25) 60)(= (make-time p26) 90)(= (make-time p27) 50)(= (make-time p28) 80)(= (make-time p29) 70)(= (make-time p30) 40)(= (make-time p31) 100)(= (make-time p32) 80)(= (make-time p33) 40)(= (make-time p34) 40)

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
(shipped o31)
(shipped o32)
(shipped o33)
(shipped o34)
))

(:metric minimize (total-cost))

)

