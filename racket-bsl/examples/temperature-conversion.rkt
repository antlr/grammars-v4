#lang htdp/bsl

(require rackunit)

(define (fahrenheit->celsius f)
    (* 5/9 (- f 32)))

(check-expect (fahrenheit->celsius 212) 100)
(check-expect (fahrenheit->celsius -40) -40)
