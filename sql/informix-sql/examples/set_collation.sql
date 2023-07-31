
SET COLLATION q'{select deptno, sum(sal) budget
     from emp
     where job = budget.job
     group by deptno}';

SET NO COLLATION;



