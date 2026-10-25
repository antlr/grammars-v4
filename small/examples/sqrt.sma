// This would be put in file sqrt.sma
// Given a positive number in d
// sqrt leaves an approximation to its sqrt in the same variable
// Uses variables x,y, e, and f
// calls abs above
d/2 -> x
{ x*x-d -> e; abs>0.00001?
  2*d ->f; x/2.0 ->y; d/f+y ->x;
}
x ->d
