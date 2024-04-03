(define (problem wood-prob) (:domain woodworking)
(:objects
	mauve - acolour
	mahogany - awood
	b0 - board
	b1 - board
	b2 - board
	b3 - board
	blue - acolour
	s9 - aboardsize
	s8 - aboardsize
	s3 - aboardsize
	s2 - aboardsize
	s1 - aboardsize
	s0 - aboardsize
	s7 - aboardsize
	s6 - aboardsize
	s5 - aboardsize
	s4 - aboardsize
	black - acolour
	s10 - aboardsize
	p2 - part
	p3 - part
	p0 - part
	p1 - part
	p7 - part
	p4 - part
	p5 - part
	p6 - part
	p8 - part
	teak - awood
	green - acolour

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
	(boardsize-successor s0 s1)
	(boardsize-successor s1 s2)
	(boardsize-successor s2 s3)
	(boardsize-successor s3 s4)
	(boardsize-successor s4 s5)
	(boardsize-successor s5 s6)
	(boardsize-successor s6 s7)
	(boardsize-successor s7 s8)
	(boardsize-successor s8 s9)
	(boardsize-successor s9 s10)
	(has-colour glazer0 mauve)
	(has-colour glazer0 natural)
	(has-colour immersion-varnisher0 green)
	(has-colour immersion-varnisher0 black)
	(has-colour immersion-varnisher0 natural)
	(has-colour immersion-varnisher0 mauve)
	(empty highspeed-saw0)
	(has-colour spray-varnisher0 green)
	(has-colour spray-varnisher0 black)
	(has-colour spray-varnisher0 natural)
	(has-colour spray-varnisher0 mauve)
	(unused p0)
	(goalsize p0 large)
	(unused p1)
	(goalsize p1 large)
	(unused p2)
	(goalsize p2 medium)
	(unused p3)
	(goalsize p3 large)
	(unused p4)
	(goalsize p4 small)
	(available p5)
	(colour p5 blue)
	(wood p5 teak)
	(surface-condition p5 rough)
	(treatment p5 glazed)
	(goalsize p5 large)
	(unused p6)
	(goalsize p6 small)
	(unused p7)
	(goalsize p7 medium)
	(unused p8)
	(goalsize p8 large)
	(boardsize b0 s10)
	(wood b0 teak)
	(surface-condition b0 rough)
	(available b0)
	(boardsize b1 s6)
	(wood b1 teak)
	(surface-condition b1 smooth)
	(available b1)
	(boardsize b2 s7)
	(wood b2 mahogany)
	(surface-condition b2 rough)
	(available b2)
	(boardsize b3 s3)
	(wood b3 mahogany)
	(surface-condition b3 smooth)
	(available b3)
	(= (total-cost) 0) 
	(= (spray-varnish-cost p0) 15) 
	(= (glaze-cost p0) 20) 
	(= (grind-cost p0) 45) 
	(= (plane-cost p0) 30) 
	(= (spray-varnish-cost p1) 15) 
	(= (glaze-cost p1) 20) 
	(= (grind-cost p1) 45) 
	(= (plane-cost p1) 30) 
	(= (spray-varnish-cost p2) 10) 
	(= (glaze-cost p2) 15) 
	(= (grind-cost p2) 30) 
	(= (plane-cost p2) 20) 
	(= (spray-varnish-cost p3) 15) 
	(= (glaze-cost p3) 20) 
	(= (grind-cost p3) 45) 
	(= (plane-cost p3) 30) 
	(= (spray-varnish-cost p4) 5) 
	(= (glaze-cost p4) 10) 
	(= (grind-cost p4) 15) 
	(= (plane-cost p4) 10) 
	(= (spray-varnish-cost p5) 15) 
	(= (glaze-cost p5) 20) 
	(= (grind-cost p5) 45) 
	(= (plane-cost p5) 30) 
	(= (spray-varnish-cost p6) 5) 
	(= (glaze-cost p6) 10) 
	(= (grind-cost p6) 15) 
	(= (plane-cost p6) 10) 
	(= (spray-varnish-cost p7) 10) 
	(= (glaze-cost p7) 15) 
	(= (grind-cost p7) 30) 
	(= (plane-cost p7) 20) 
	(= (spray-varnish-cost p8) 15) 
	(= (glaze-cost p8) 20) 
	(= (grind-cost p8) 45) 
	(= (plane-cost p8) 30) 
)
(:goal
	(and
		(available p0)
		(colour p0 green)
		(treatment p0 varnished)
		(available p1)
		(colour p1 black)
		(surface-condition p1 verysmooth)
		(treatment p1 varnished)
		(available p2)
		(colour p2 mauve)
		(wood p2 teak)
		(available p3)
		(surface-condition p3 verysmooth)
		(treatment p3 glazed)
		(available p4)
		(wood p4 mahogany)
		(treatment p4 varnished)
		(available p5)
		(surface-condition p5 verysmooth)
		(treatment p5 varnished)
		(available p6)
		(wood p6 mahogany)
		(treatment p6 varnished)
		(available p7)
		(colour p7 natural)
		(wood p7 mahogany)
		(surface-condition p7 verysmooth)
		(treatment p7 varnished)
		(available p8)
		(colour p8 natural)
		(surface-condition p8 smooth)
		(treatment p8 glazed)
	)
)
(:metric minimize (total-cost))
)
