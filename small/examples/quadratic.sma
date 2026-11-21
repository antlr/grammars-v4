//This would be in a file quadratic.sma
//'small quadratic' then reads three lines containing a number each
//and solves a quadratic equation ax*x+bx+c=0
// Uses variables a,b,c,d,e,f,s,t,x,y
// Calls sqrt and indirectly abs

//read a,b, and c
$->a; $->b; $->c;
// calculate the denominator t
2*a->t
[  t=0? "This is a linear equation"!
|| t<>0?// a<>0.
	// find the discriminant d
	2*t*c->s; b*b-s->d;
	// select the correct case depending on the discriminant d
	[ d>0? sqrt; "Real roots: " -b+d/t " and " -b-d/t !
	||d=0? "Equal roots: " -b/t!
	||d<0? -d ->d; sqrt; "Complex roots: " -b/t "+/- i*" d/t!
	]
]
