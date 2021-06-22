begin


procedure bairstow (n, a, eps0, eps1, eps2, eps3, K, m, x, y, nat, ex);
    value n, a, eps0, eps1, eps2, eps3, K; integer n, K, m;
    integer array nat, ex; real eps0, eps1, eps2, eps3;
    real array a,x,y;

comment		The Bairstow-Hitchcock iteration is used to find
    successively pairs of roots of a polynomial equation of degree n
    with coefficients a[i] (i = 0, 1, ... n), where a[n] is the
    constant term. On exit from the procedure, m is the number of pairs
    of roots found, x[i] and y[i] (i=1, ..., m) are a pair of real roots
    if nat[i] = 1, the real and imaginary parts of a complex pair if
    nat[i] = -1, and ex[i] indicates which of the following conditions
    was met to exit from the iteration loop in finding this pair:
	1. Remainders, r1, r0, become absolutely less than eps1.
	2. Corrections, incrp, incrq, become absolutely less than  eps2.
	3. The ratios, incrp/p, incrq/q, become absolutely less than eps3.
	4. The number of iterations becomes K.
    In the last case, the pair of roots found is not reliable and no further
    effort to find additionalroots is made. The quantity eps0 is used
    as a lower bound for the denominator in the expressions from which
    incrp and incrq are found.;

begin
    integer i, j, k, n1, n2, m1;
    real r0,s0,v0,det0,r1,s1,v1,det1,det2,p,q,incrp,incrq,S,T;
    real array b,c[0:n+1];

    for i := 0 step 1 until n do b[i] := a[i];
    b[n+1] := 0; n2 := entier((n + 1) / 2); n1 := 2 * n2;
    for m1 := 1 step 1 until n2 do begin
	p := 0 ; q := 0;
	for k := 1 step 1 until K do begin

stp:	    for i := 0 step 1 until n1 do c[i] := b[i];
	    for j := n1 - 2, n1 - 4 do begin
		for i := 0 step 1 until j do begin
		    c[i+1] := c[i+1] - p * c[i];
		    c[i+2] := c[i+2] - q * c[i];
		end j;
	    end i;
	    r0 := c[n1] ; r1 := c[n1 - 1];
	    s0 := c[n1 - 2] ; s1 := c[n1 - 3];
	    v0 := -q * s1 ; v1 := s0 - s1 * p;
	    det0 := v1 * s0 - v0 * s1;

	    if  abs(det0) < eps0 then begin
		p := p + 1 ; q := q + 1 ; goto stp;
	    end;
	    det1 := s0 * r1 - s1 * r0;
	    det2 := r0 * v1 - v0 * r1;
	    incrp := det1 / det0 ; incrq := det2 / det0;
	    p := p + incrp ; q := q + incrq;

	    if  abs(r0) < eps1 then begin
		if abs(r1) < eps1 then begin
		    ex[m1] := 1 ; goto next;
		end;
	    end;

	    if abs(incrp) < eps2 then begin
		if abs(incrq) < eps2 then begin
		    ex[m1] := 2 ; goto next;
		end;
	    end;

	    if abs(incrp / p) < eps3 then begin
		if abs(incrq / q) < eps3 then begin
		    ex[m1] := 3 ; goto next;
		end;
	    end;
	end k;

	ex[m1] := 4;
next:	S := -p/2 ; T := (S * S) - q;
	if T >= 0 then begin
	    T := sqrt(T);
	    nat[m1] := 1 ; x[m1] := S + T; y[m1] := S - T;
	end;
	if T < 0 then begin
	    nat[m1] := -1 ; x[m1] := S; y[m1] := sqrt(-T);
	end;

	if ex[m1] = 4 then goto out;

	for j := 0 step 1 until (n1 - 2) do begin
	    b[j+1] := b[j+1] - p * b[j];
	    b[j+2] := b[j+2] - q * b[j];
	end j;
	n1 := n1 - 2 ; if n1 < 1 then begin
out:	    m := m1;
	    goto out2;
	end;
	if n1 < 3 then begin
	    m1 := m1 + 1 ; ex[m1] := 1;
	    p := b[1] / b[0] ; q := b[2] / b[0];
	    goto next;
	end;
    end;
out2: end bairstow;


procedure printroots(m, nat, ex, x, y, ALARM);
value m; integer m;
integer array nat, ex; real array x, y;
label ALARM;
begin
    integer i;
    outstring(1,"Roots found by Bairstow's method.\ni\tex\tnat\tx\ty\n");
    for i:=1 step 1 until m do begin
	outinteger(1,i);
	outstring(1,"\t");
	outinteger(1,ex[i]);
	outstring(1,"\t");
	outinteger(1,nat[i]);
	outstring(1,"\t");
	outreal(1,x[i]);
	outreal(1,y[i]);
	outstring(1,"\n");
	if ex[i] = 4 then goto ALARM;
    end i;
end printroots;

    integer n, K, m;
    integer array nat[1:6],ex[1:6];
    real eps0, eps1, eps2, eps3;
    real array a[0:6], x[1:6], y[1:6];

    eps0 := 1e-8; eps1 := eps2 := eps3 := 1e-15;
    K:=20;

    comment First example from certification by Tatcher;
    n:=4;
    a[0] := 1; a[1] := -3; a[2] := 20; a[3] := 44; a[4] := 54;
    bairstow(n, a, eps0, eps1, eps2, eps3, K, m, x, y, nat, ex);
    printroots(m, nat, ex, x, y, ALARM);

    comment Second example from certification by Tatcher;
    n:=6;
    a[0] := 1; a[1] := -2; a[2] := 2; a[3] := 1 ; a[4] := 6; a[5] := -6; a[6] := 8;
    bairstow(n, a, eps0, eps1, eps2, eps3, K, m, x, y, nat, ex);
    printroots(m, nat, ex, x, y, ALARM);


    goto DONE;
ALARM:	outstring(1,"Bairstow failed!\n");

DONE:	outstring(1,"Program end.\n");

end
