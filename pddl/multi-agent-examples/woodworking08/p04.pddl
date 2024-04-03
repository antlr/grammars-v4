(define (problem wood-prob) (:domain woodworking)
(:objects
	mauve - acolour
	b0 - board
	pine - awood
	b2 - board
	b3 - board
	blue - acolour
	s3 - aboardsize
	s2 - aboardsize
	s1 - aboardsize
	s0 - aboardsize
	oak - awood
	s6 - aboardsize
	s5 - aboardsize
	s4 - aboardsize
	white - acolour
	red - acolour
	p10 - part
	p11 - part
	beech - awood
	p2 - part
	p3 - part
	p0 - part
	p1 - part
	p7 - part
	p4 - part
	p5 - part
	p6 - part
	p8 - part
	p9 - part
	s7 - aboardsize
	b1 - board

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
	(has-colour glazer0 white)
	(has-colour glazer0 red)
	(has-colour immersion-varnisher0 mauve)
	(has-colour immersion-varnisher0 white)
	(has-colour immersion-varnisher0 natural)
	(has-colour immersion-varnisher0 red)
	(empty highspeed-saw0)
	(has-colour spray-varnisher0 mauve)
	(has-colour spray-varnisher0 white)
	(has-colour spray-varnisher0 natural)
	(has-colour spray-varnisher0 red)
	(unused p0)
	(goalsize p0 medium)
	(unused p1)
	(goalsize p1 medium)
	(unused p2)
	(goalsize p2 small)
	(unused p3)
	(goalsize p3 small)
	(unused p4)
	(goalsize p4 large)
	(unused p5)
	(goalsize p5 large)
	(unused p6)
	(goalsize p6 medium)
	(available p7)
	(colour p7 blue)
	(wood p7 oak)
	(surface-condition p7 rough)
	(treatment p7 varnished)
	(goalsize p7 medium)
	(available p8)
	(colour p8 blue)
	(wood p8 beech)
	(surface-condition p8 smooth)
	(treatment p8 glazed)
	(goalsize p8 large)
	(unused p9)
	(goalsize p9 small)
	(unused p10)
	(goalsize p10 medium)
	(unused p11)
	(goalsize p11 small)
	(boardsize b0 s6)
	(wood b0 beech)
	(surface-condition b0 rough)
	(available b0)
	(boardsize b1 s7)
	(wood b1 beech)
	(surface-condition b1 rough)
	(available b1)
	(boardsize b2 s6)
	(wood b2 oak)
	(surface-condition b2 rough)
	(available b2)
	(boardsize b3 s7)
	(wood b3 pine)
	(surface-condition b3 rough)
	(available b3)
	(= (total-cost) 0) 
	(= (spray-varnish-cost p0) 10) 
	(= (glaze-cost p0) 15) 
	(= (grind-cost p0) 30) 
	(= (plane-cost p0) 20) 
	(= (spray-varnish-cost p1) 10) 
	(= (glaze-cost p1) 15) 
	(= (grind-cost p1) 30) 
	(= (plane-cost p1) 20) 
	(= (spray-varnish-cost p2) 5) 
	(= (glaze-cost p2) 10) 
	(= (grind-cost p2) 15) 
	(= (plane-cost p2) 10) 
	(= (spray-varnish-cost p3) 5) 
	(= (glaze-cost p3) 10) 
	(= (grind-cost p3) 15) 
	(= (plane-cost p3) 10) 
	(= (spray-varnish-cost p4) 15) 
	(= (glaze-cost p4) 20) 
	(= (grind-cost p4) 45) 
	(= (plane-cost p4) 30) 
	(= (spray-varnish-cost p5) 15) 
	(= (glaze-cost p5) 20) 
	(= (grind-cost p5) 45) 
	(= (plane-cost p5) 30) 
	(= (spray-varnish-cost p6) 10) 
	(= (glaze-cost p6) 15) 
	(= (grind-cost p6) 30) 
	(= (plane-cost p6) 20) 
	(= (spray-varnish-cost p7) 10) 
	(= (glaze-cost p7) 15) 
	(= (grind-cost p7) 30) 
	(= (plane-cost p7) 20) 
	(= (spray-varnish-cost p8) 15) 
	(= (glaze-cost p8) 20) 
	(= (grind-cost p8) 45) 
	(= (plane-cost p8) 30) 
	(= (spray-varnish-cost p9) 5) 
	(= (glaze-cost p9) 10) 
	(= (grind-cost p9) 15) 
	(= (plane-cost p9) 10) 
	(= (spray-varnish-cost p10) 10) 
	(= (glaze-cost p10) 15) 
	(= (grind-cost p10) 30) 
	(= (plane-cost p10) 20) 
	(= (spray-varnish-cost p11) 5) 
	(= (glaze-cost p11) 10) 
	(= (grind-cost p11) 15) 
	(= (plane-cost p11) 10) 
)
(:goal
	(and
		(available p0)
		(colour p0 red)
		(wood p0 beech)
		(surface-condition p0 verysmooth)
		(treatment p0 glazed)
		(available p1)
		(wood p1 pine)
		(surface-condition p1 verysmooth)
		(available p2)
		(surface-condition p2 verysmooth)
		(treatment p2 glazed)
		(available p3)
		(colour p3 natural)
		(wood p3 beech)
		(surface-condition p3 verysmooth)
		(treatment p3 varnished)
		(available p4)
		(colour p4 natural)
		(wood p4 oak)
		(surface-condition p4 smooth)
		(treatment p4 varnished)
		(available p5)
		(colour p5 red)
		(surface-condition p5 smooth)
		(available p6)
		(colour p6 natural)
		(wood p6 beech)
		(surface-condition p6 verysmooth)
		(treatment p6 varnished)
		(available p7)
		(colour p7 mauve)
		(surface-condition p7 smooth)
		(treatment p7 varnished)
		(available p8)
		(colour p8 white)
		(wood p8 beech)
		(surface-condition p8 verysmooth)
		(available p9)
		(surface-condition p9 verysmooth)
		(treatment p9 varnished)
		(available p10)
		(wood p10 pine)
		(surface-condition p10 verysmooth)
		(treatment p10 glazed)
		(available p11)
		(colour p11 red)
		(wood p11 oak)
		(surface-condition p11 verysmooth)
		(treatment p11 glazed)
	)
)
(:metric minimize (total-cost))
)
