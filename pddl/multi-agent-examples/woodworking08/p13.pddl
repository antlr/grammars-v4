; woodworking task with 9 parts and 120% wood
; Machines:
;   1 grinder
;   1 glazer
;   1 immersion-varnisher
;   1 planer
;   1 highspeed-saw
;   1 spray-varnisher
;   1 saw
; random seed: 958211

(define (problem wood-prob)
  (:domain woodworking)
  (:objects
    green mauve blue red black white - acolour
    cherry mahogany - awood
    p0 p1 p2 p3 p4 p5 p6 p7 p8 - part
    b0 b1 b2 - board
    s0 s1 s2 s3 s4 s5 s6 s7 s8 s9 - aboardsize
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
    (boardsize-successor s0 s1)
    (boardsize-successor s1 s2)
    (boardsize-successor s2 s3)
    (boardsize-successor s3 s4)
    (boardsize-successor s4 s5)
    (boardsize-successor s5 s6)
    (boardsize-successor s6 s7)
    (boardsize-successor s7 s8)
    (boardsize-successor s8 s9)
    (has-colour glazer0 blue)
    (has-colour glazer0 white)
    (has-colour immersion-varnisher0 blue)
    (has-colour immersion-varnisher0 white)
    (has-colour immersion-varnisher0 black)
    (has-colour immersion-varnisher0 red)
    (empty highspeed-saw0)
    (has-colour spray-varnisher0 blue)
    (has-colour spray-varnisher0 white)
    (has-colour spray-varnisher0 black)
    (has-colour spray-varnisher0 red)
    (unused p0)
    (goalsize p0 large)
    (= (spray-varnish-cost p0) 15)
    (= (glaze-cost p0) 20)
    (= (grind-cost p0) 45)
    (= (plane-cost p0) 30)
    (unused p1)
    (goalsize p1 medium)
    (= (spray-varnish-cost p1) 10)
    (= (glaze-cost p1) 15)
    (= (grind-cost p1) 30)
    (= (plane-cost p1) 20)
    (unused p2)
    (goalsize p2 small)
    (= (spray-varnish-cost p2) 5)
    (= (glaze-cost p2) 10)
    (= (grind-cost p2) 15)
    (= (plane-cost p2) 10)
    (unused p3)
    (goalsize p3 medium)
    (= (spray-varnish-cost p3) 10)
    (= (glaze-cost p3) 15)
    (= (grind-cost p3) 30)
    (= (plane-cost p3) 20)
    (unused p4)
    (goalsize p4 small)
    (= (spray-varnish-cost p4) 5)
    (= (glaze-cost p4) 10)
    (= (grind-cost p4) 15)
    (= (plane-cost p4) 10)
    (unused p5)
    (goalsize p5 small)
    (= (spray-varnish-cost p5) 5)
    (= (glaze-cost p5) 10)
    (= (grind-cost p5) 15)
    (= (plane-cost p5) 10)
    (unused p6)
    (goalsize p6 medium)
    (= (spray-varnish-cost p6) 10)
    (= (glaze-cost p6) 15)
    (= (grind-cost p6) 30)
    (= (plane-cost p6) 20)
    (unused p7)
    (goalsize p7 large)
    (= (spray-varnish-cost p7) 15)
    (= (glaze-cost p7) 20)
    (= (grind-cost p7) 45)
    (= (plane-cost p7) 30)
    (unused p8)
    (goalsize p8 large)
    (= (spray-varnish-cost p8) 15)
    (= (glaze-cost p8) 20)
    (= (grind-cost p8) 45)
    (= (plane-cost p8) 30)
    (boardsize b0 s9)
    (wood b0 cherry)
    (surface-condition b0 rough)
    (available b0)
    (boardsize b1 s7)
    (wood b1 mahogany)
    (surface-condition b1 rough)
    (available b1)
    (boardsize b2 s7)
    (wood b2 mahogany)
    (surface-condition b2 rough)
    (available b2)
  )
  (:goal
    (and
      (available p0)
      (colour p0 red)
      (wood p0 cherry)
      (surface-condition p0 verysmooth)
      (treatment p0 varnished)
      (available p1)
      (colour p1 blue)
      (surface-condition p1 verysmooth)
      (available p2)
      (colour p2 white)
      (wood p2 cherry)
      (surface-condition p2 smooth)
      (treatment p2 varnished)
      (available p3)
      (surface-condition p3 smooth)
      (treatment p3 glazed)
      (available p4)
      (colour p4 black)
      (wood p4 mahogany)
      (surface-condition p4 smooth)
      (treatment p4 varnished)
      (available p5)
      (colour p5 white)
      (wood p5 mahogany)
      (available p6)
      (colour p6 white)
      (treatment p6 glazed)
      (available p7)
      (wood p7 cherry)
      (surface-condition p7 smooth)
      (available p8)
      (wood p8 mahogany)
      (treatment p8 glazed)
    )
  )
  (:metric minimize (total-cost))
)
