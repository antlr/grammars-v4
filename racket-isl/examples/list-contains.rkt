#lang htdp/isl

(require rackunit)

(define contains (lambda (l item) 
                   (if (equal? l '()) 
                        #f
                        (if (equal? item (first l)) 
                            #t
                            (contains (rest l) item)))))

(define z (list 1 2 3))
(check-expect (contains 1 z) #true)
(check-expect (contains 4 z) #false)
