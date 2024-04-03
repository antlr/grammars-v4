


(define (problem mixed-f4-p2-u0-v0-g0-a0-n0-A0-B0-N0-F0-r0)
   (:domain miconic)
   (:objects p0 p1 - passenger
             f0 f1 f2 f3 - floor)


(:init
(above f0 f1)
(above f0 f2)
(above f0 f3)

(above f1 f2)
(above f1 f3)

(above f2 f3)



(origin p0 f3)
(destin p0 f2)

(origin p1 f1)
(destin p1 f3)






(lift-at f0)
)


(:goal (and 
(served p0)
(served p1)
))
)


