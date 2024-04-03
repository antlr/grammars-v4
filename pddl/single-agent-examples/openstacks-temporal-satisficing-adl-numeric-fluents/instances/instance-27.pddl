(define (problem os-time-p31_1)
(:domain openstacks-time-numeric-ADL)
(:objects 
o1 o2 o3 o4 o5 o6 o7 o8 o9 o10 o11 o12 o13 o14 o15 o16 o17 o18 o19 o20 o21 o22 o23 o24 o25 o26 o27 o28 o29 o30 o31  - order
p1 p2 p3 p4 p5 p6 p7 p8 p9 p10 p11 p12 p13 p14 p15 p16 p17 p18 p19 p20 p21 p22 p23 p24 p25 p26 p27 p28 p29 p30 p31  - product

)

(:init
(= (stacks-in-use) 0)
(= (max-stacks) 27)

(waiting o1)
(includes o1 p6)

(waiting o2)
(includes o2 p22)(includes o2 p25)(includes o2 p30)

(waiting o3)
(includes o3 p7)(includes o3 p15)(includes o3 p16)

(waiting o4)
(includes o4 p11)(includes o4 p17)

(waiting o5)
(includes o5 p20)(includes o5 p24)(includes o5 p28)

(waiting o6)
(includes o6 p15)

(waiting o7)
(includes o7 p29)

(waiting o8)
(includes o8 p4)(includes o8 p20)

(waiting o9)
(includes o9 p1)(includes o9 p18)

(waiting o10)
(includes o10 p21)(includes o10 p25)

(waiting o11)
(includes o11 p21)

(waiting o12)
(includes o12 p24)

(waiting o13)
(includes o13 p3)(includes o13 p11)

(waiting o14)
(includes o14 p14)(includes o14 p16)(includes o14 p20)

(waiting o15)
(includes o15 p8)

(waiting o16)
(includes o16 p7)

(waiting o17)
(includes o17 p21)

(waiting o18)
(includes o18 p5)(includes o18 p8)(includes o18 p9)(includes o18 p10)(includes o18 p11)(includes o18 p17)

(waiting o19)
(includes o19 p9)

(waiting o20)
(includes o20 p13)(includes o20 p27)

(waiting o21)
(includes o21 p11)

(waiting o22)
(includes o22 p24)

(waiting o23)
(includes o23 p7)(includes o23 p17)(includes o23 p19)

(waiting o24)
(includes o24 p7)(includes o24 p11)(includes o24 p12)(includes o24 p31)

(waiting o25)
(includes o25 p3)

(waiting o26)
(includes o26 p18)(includes o26 p23)(includes o26 p25)

(waiting o27)
(includes o27 p18)(includes o27 p20)

(waiting o28)
(includes o28 p26)

(waiting o29)
(includes o29 p9)

(waiting o30)
(includes o30 p1)(includes o30 p3)(includes o30 p8)

(waiting o31)
(includes o31 p2)(includes o31 p25)

(= (make-time p1) 50)(= (make-time p2) 10)(= (make-time p3) 40)(= (make-time p4) 40)(= (make-time p5) 10)(= (make-time p6) 20)(= (make-time p7) 70)(= (make-time p8) 60)(= (make-time p9) 40)(= (make-time p10) 20)(= (make-time p11) 50)(= (make-time p12) 40)(= (make-time p13) 60)(= (make-time p14) 100)(= (make-time p15) 10)(= (make-time p16) 60)(= (make-time p17) 60)(= (make-time p18) 20)(= (make-time p19) 80)(= (make-time p20) 10)(= (make-time p21) 40)(= (make-time p22) 100)(= (make-time p23) 30)(= (make-time p24) 40)(= (make-time p25) 30)(= (make-time p26) 80)(= (make-time p27) 60)(= (make-time p28) 10)(= (make-time p29) 10)(= (make-time p30) 10)(= (make-time p31) 80)

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
))

(:metric minimize (total-time))

)

