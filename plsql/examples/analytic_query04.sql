select deptno
	, ename
	, hiredate
	, listagg(ename, ',') within group (order by hiredate) over (partition by deptno) as employees
from emp