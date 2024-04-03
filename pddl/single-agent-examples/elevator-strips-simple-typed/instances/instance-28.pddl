


(define (problem mixed-f12-p6-u0-v0-g0-a0-n0-A0-B0-N0-F0-r2)
   (:domain miconic)
   (:objects p0 p1 p2 p3 p4 p5 - passenger
             f0 f1 f2 f3 f4 f5 f6 f7 f8 f9 
             f10 f11 - floor)


(:init
(above f0 f1)
(above f0 f2)
(above f0 f3)
(above f0 f4)
(above f0 f5)
(above f0 f6)
(above f0 f7)
(above f0 f8)
(above f0 f9)
(above f0 f10)
(above f0 f11)

(above f1 f2)
(above f1 f3)
(above f1 f4)
(above f1 f5)
(above f1 f6)
(above f1 f7)
(above f1 f8)
(above f1 f9)
(above f1 f10)
(above f1 f11)

(above f2 f3)
(above f2 f4)
(above f2 f5)
(above f2 f6)
(above f2 f7)
(above f2 f8)
(above f2 f9)
(above f2 f10)
(above f2 f11)

(above f3 f4)
(above f3 f5)
(above f3 f6)
(above f3 f7)
(above f3 f8)
(above f3 f9)
(above f3 f10)
(above f3 f11)

(above f4 f5)
(above f4 f6)
(above f4 f7)
(above f4 f8)
(above f4 f9)
(above f4 f10)
(above f4 f11)

(above f5 f6)
(above f5 f7)
(above f5 f8)
(above f5 f9)
(above f5 f10)
(above f5 f11)

(above f6 f7)
(above f6 f8)
(above f6 f9)
(above f6 f10)
(above f6 f11)

(above f7 f8)
(above f7 f9)
(above f7 f10)
(above f7 f11)

(above f8 f9)
(above f8 f10)
(above f8 f11)

(above f9 f10)
(above f9 f11)

(above f10 f11)



(origin p0 f7)
(destin p0 f0)

(origin p1 f4)
(destin p1 f6)

(origin p2 f9)
(destin p2 f6)

(origin p3 f2)
(destin p3 f7)

(origin p4 f3)
(destin p4 f2)

(origin p5 f11)
(destin p5 f7)






(lift-at f0)
)


(:goal (and 
(served p0)
(served p1)
(served p2)
(served p3)
(served p4)
(served p5)
))
)


