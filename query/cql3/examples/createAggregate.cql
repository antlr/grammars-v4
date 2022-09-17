CREATE AGGREGATE cycling.average(int) 
SFUNC avgState 
STYPE tuple<int,bigint> 
FINALFUNC avgFinal 
INITCOND (0,0);