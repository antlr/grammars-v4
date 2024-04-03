(define (problem wood-prob) (:domain woodworking)
(:objects
	blue - acolour
	mauve - acolour
	mahogany - awood
	b0 - board
	walnut - awood
	p2 - part
	p3 - part
	p0 - part
	p1 - part
	p4 - part
	p5 - part
	s3 - aboardsize
	s2 - aboardsize
	s1 - aboardsize
	s0 - aboardsize
	s6 - aboardsize
	s5 - aboardsize
	s4 - aboardsize
	black - acolour

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
	(has-colour glazer0 blue)
	(has-colour immersion-varnisher0 mauve)
	(has-colour immersion-varnisher0 black)
	(empty highspeed-saw0)
	(has-colour spray-varnisher0 mauve)
	(has-colour spray-varnisher0 black)
	(available p0)
	(colour p0 natural)
	(wood p0 walnut)
	(surface-condition p0 verysmooth)
	(treatment p0 glazed)
	(goalsize p0 small)
	(unused p1)
	(goalsize p1 medium)
	(available p2)
	(colour p2 black)
	(wood p2 mahogany)
	(surface-condition p2 rough)
	(treatment p2 glazed)
	(goalsize p2 large)
	(unused p3)
	(goalsize p3 medium)
	(available p4)
	(colour p4 black)
	(wood p4 mahogany)
	(surface-condition p4 verysmooth)
	(treatment p4 varnished)
	(goalsize p4 large)
	(available p5)
	(colour p5 mauve)
	(wood p5 walnut)
	(surface-condition p5 rough)
	(treatment p5 glazed)
	(goalsize p5 small)
	(boardsize b0 s6)
	(wood b0 walnut)
	(surface-condition b0 smooth)
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
	(= (spray-varnish-cost p3) 10) 
	(= (glaze-cost p3) 15) 
	(= (grind-cost p3) 30) 
	(= (plane-cost p3) 20) 
	(= (spray-varnish-cost p4) 15) 
	(= (glaze-cost p4) 20) 
	(= (grind-cost p4) 45) 
	(= (plane-cost p4) 30) 
	(= (spray-varnish-cost p5) 5) 
	(= (glaze-cost p5) 10) 
	(= (grind-cost p5) 15) 
	(= (plane-cost p5) 10) 
)
(:goal
	(and
		(available p0)
		(colour p0 black)
		(treatment p0 varnished)
		(available p1)
		(wood p1 walnut)
		(surface-condition p1 smooth)
		(treatment p1 glazed)
		(available p2)
		(colour p2 blue)
		(surface-condition p2 verysmooth)
		(treatment p2 glazed)
		(available p3)
		(surface-condition p3 smooth)
		(treatment p3 varnished)
		(available p4)
		(colour p4 mauve)
		(wood p4 mahogany)
		(surface-condition p4 smooth)
		(treatment p4 varnished)
		(available p5)
		(surface-condition p5 verysmooth)
		(treatment p5 varnished)
	)
)
(:metric minimize (total-cost))
)
