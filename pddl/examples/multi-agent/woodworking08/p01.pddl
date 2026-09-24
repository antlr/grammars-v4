(define (problem wood-prob) (:domain woodworking)
(:objects
	b0 - board
	p2 - part
	p0 - part
	p1 - part
	s3 - aboardsize
	s2 - aboardsize
	s1 - aboardsize
	s0 - aboardsize
	pine - awood
	teak - awood
	red - acolour

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
	(has-colour glazer0 natural)
	(has-colour glazer0 red)
	(has-colour immersion-varnisher0 natural)
	(has-colour immersion-varnisher0 red)
	(empty highspeed-saw0)
	(has-colour spray-varnisher0 natural)
	(has-colour spray-varnisher0 red)
	(available p0)
	(colour p0 red)
	(wood p0 pine)
	(surface-condition p0 smooth)
	(treatment p0 varnished)
	(goalsize p0 small)
	(unused p1)
	(goalsize p1 medium)
	(available p2)
	(colour p2 natural)
	(wood p2 teak)
	(surface-condition p2 verysmooth)
	(treatment p2 varnished)
	(goalsize p2 large)
	(boardsize b0 s3)
	(wood b0 pine)
	(surface-condition b0 rough)
	(available b0)
	(= (total-cost) 0) 
	(= (spray-varnish-cost p0) 5) 
	(= (glaze-cost p0) 10) 
	(= (grind-cost p0) 15) 
	(= (plane-cost p0) 10) 
	(= (spray-varnish-cost p1) 10) 
	(= (glaze-cost p1) 15) 
	(= (grind-cost p1) 30) 
	(= (plane-cost p1) 20) 
	(= (spray-varnish-cost p2) 15) 
	(= (glaze-cost p2) 20) 
	(= (grind-cost p2) 45) 
	(= (plane-cost p2) 30) 
)
(:goal
	(and
		(available p0)
		(colour p0 natural)
		(wood p0 pine)
		(available p1)
		(colour p1 natural)
		(wood p1 pine)
		(surface-condition p1 smooth)
		(treatment p1 varnished)
		(available p2)
		(colour p2 red)
		(wood p2 teak)
	)
)
(:metric minimize (total-cost))
)
