FIBONACCI ; compute the Fibonacci series
 SET (A,B)=1
 FOR I=1:1 SET S=A+B WRITE !,S SET A=B SET B=S QUIT:S>100
 WRITE !,"Result= ",S
