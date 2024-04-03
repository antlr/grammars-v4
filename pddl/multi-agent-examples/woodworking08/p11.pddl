; woodworking task with 3 parts and 120% wood
; Machines:
;   1 grinder
;   1 glazer
;   1 immersion-varnisher
;   1 planer
;   1 highspeed-saw
;   1 spray-varnisher
;   1 saw
; random seed: 578239

(define (problem wood-prob)
  (:domain woodworking)
  (:objects
    blue mauve - acolour
    beech mahogany - awood
    p0 p1 p2 - part
     - board
    s0 - aboardsize
	(:private grinder0
		grinder0 - grinder
	)
	
	(:private glazer0
		glazer0 - glazer
	)

	(:private planer0
		planer0 - planer
	)

	(:private immersion-varnisher0
		immersion-varnisher0 - immersion-varnisher
	)

	(:private highspeed-saw0
		highspeed-saw0 - highspeed-saw
	)

	(:private saw0
		saw0 - saw
	)

	(:private spray-varnisher0
		spray-varnisher0 - spray-varnisher
	)
  )
  (:init
    (grind-treatment-change grinder0 varnished colourfragments)
    (grind-treatment-change grinder0 glazed untreated)
    (grind-treatment-change grinder0 untreated untreated)
    (grind-treatment-change grinder0 colourfragments untreated)
    (is-smooth smooth)
    (is-smooth verysmooth)
    (= (total-cost) 0)
    (has-colour glazer0 natural)
    (has-colour immersion-varnisher0 blue)
    (empty highspeed-saw0)
    (has-colour spray-varnisher0 mauve)
    (available p0)
    (colour p0 blue)
    (wood p0 mahogany)
    (surface-condition p0 verysmooth)
    (treatment p0 colourfragments)
    (goalsize p0 small)
    (= (spray-varnish-cost p0) 5)
    (= (glaze-cost p0) 10)
    (= (grind-cost p0) 15)
    (= (plane-cost p0) 10)
    (available p1)
    (colour p1 natural)
    (wood p1 mahogany)
    (surface-condition p1 smooth)
    (treatment p1 colourfragments)
    (goalsize p1 small)
    (= (spray-varnish-cost p1) 5)
    (= (glaze-cost p1) 10)
    (= (grind-cost p1) 15)
    (= (plane-cost p1) 10)
    (available p2)
    (colour p2 mauve)
    (wood p2 beech)
    (surface-condition p2 verysmooth)
    (treatment p2 colourfragments)
    (goalsize p2 small)
    (= (spray-varnish-cost p2) 5)
    (= (glaze-cost p2) 10)
    (= (grind-cost p2) 15)
    (= (plane-cost p2) 10)
  )
  (:goal
    (and
      (available p0)
      (wood p0 mahogany)
      (surface-condition p0 smooth)
      (available p1)
      (surface-condition p1 smooth)
      (treatment p1 glazed)
      (available p2)
      (colour p2 natural)
      (treatment p2 glazed)
    )
  )
  (:metric minimize (total-cost))
)
