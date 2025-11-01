/*
*
* Original Javascript version by David Hedbor(http://www.bagley.org/~doug/shootout/)
*
*/

function Ack(M, N) {
    if (M == 0) return( N + 1 );
    if (N == 0) return( Ack(M - 1, 1) );
    return( Ack(M - 1, Ack(M, (N - 1))) );
}

local n;

if(vargv.len()!=0) {
   n = vargv[0].tointeger();
  if(n < 1) n = 1;
} else {
  n = 1;
}
print("n="+n+"\n");
print("Ack(3,"+ n+ "):"+ Ack(3, n));

