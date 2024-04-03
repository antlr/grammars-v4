(define (problem os-time-p32_1)
(:domain openstacks-time-numeric-ADL)
(:objects 
o1 o2 o3 o4 o5 o6 o7 o8 o9 o10 o11 o12 o13 o14 o15 o16 o17 o18 o19 o20 o21 o22 o23 o24 o25 o26 o27 o28 o29 o30 o31 o32  - order
p1 p2 p3 p4 p5 p6 p7 p8 p9 p10 p11 p12 p13 p14 p15 p16 p17 p18 p19 p20 p21 p22 p23 p24 p25 p26 p27 p28 p29 p30 p31 p32  - product

)

(:init
(= (stacks-in-use) 0)
(= (max-stacks) 28)

(waiting o1)
(includes o1 p30)

(waiting o2)
(includes o2 p20)(includes o2 p29)

(waiting o3)
(includes o3 p4)(includes o3 p20)

(waiting o4)
(includes o4 p9)(includes o4 p18)(includes o4 p23)

(waiting o5)
(includes o5 p17)(includes o5 p19)(includes o5 p26)

(waiting o6)
(includes o6 p9)(includes o6 p13)

(waiting o7)
(includes o7 p11)(includes o7 p14)

(waiting o8)
(includes o8 p19)

(waiting o9)
(includes o9 p2)(includes o9 p12)

(waiting o10)
(includes o10 p24)

(waiting o11)
(includes o11 p25)(includes o11 p28)(includes o11 p30)

(waiting o12)
(includes o12 p8)(includes o12 p17)

(waiting o13)
(includes o13 p2)

(waiting o14)
(includes o14 p15)(includes o14 p24)(includes o14 p29)

(waiting o15)
(includes o15 p26)

(waiting o16)
(includes o16 p16)

(waiting o17)
(includes o17 p10)(includes o17 p13)(includes o17 p27)

(waiting o18)
(includes o18 p29)

(waiting o19)
(includes o19 p14)

(waiting o20)
(includes o20 p7)

(waiting o21)
(includes o21 p6)

(waiting o22)
(includes o22 p1)(includes o22 p5)(includes o22 p12)

(waiting o23)
(includes o23 p21)(includes o23 p30)(includes o23 p32)

(waiting o24)
(includes o24 p3)

(waiting o25)
(includes o25 p13)

(waiting o26)
(includes o26 p22)

(waiting o27)
(includes o27 p30)

(waiting o28)
(includes o28 p28)

(waiting o29)
(includes o29 p26)(includes o29 p31)(includes o29 p32)

(waiting o30)
(includes o30 p16)

(waiting o31)
(includes o31 p2)(includes o31 p9)

(waiting o32)
(includes o32 p16)

(= (make-time p1) 40)(= (make-time p2) 70)(= (make-time p3) 60)(= (make-time p4) 40)(= (make-time p5) 20)(= (make-time p6) 50)(= (make-time p7) 100)(= (make-time p8) 90)(= (make-time p9) 80)(= (make-time p10) 20)(= (make-time p11) 10)(= (make-time p12) 70)(= (make-time p13) 40)(= (make-time p14) 90)(= (make-time p15) 20)(= (make-time p16) 50)(= (make-time p17) 20)(= (make-time p18) 20)(= (make-time p19) 80)(= (make-time p20) 90)(= (make-time p21) 100)(= (make-time p22) 60)(= (make-time p23) 100)(= (make-time p24) 40)(= (make-time p25) 90)(= (make-time p26) 20)(= (make-time p27) 50)(= (make-time p28) 50)(= (make-time p29) 70)(= (make-time p30) 10)(= (make-time p31) 70)(= (make-time p32) 50)

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
))

(:metric minimize (total-time))

)

