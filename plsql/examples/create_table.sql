create table test.my_table ( A number(38,0), junk varchar2(30) ) 
PCTFREE 10 
INITRANS 4
STORAGE ( INITIAL 10M
	  NEXT 10M
	)
TABLESPACE TBS_HUGE;

create global temporary table test.my_table ( A number(38,0), junk varchar2(30) );

create table test.my_table ( A number(38,0), junk varchar2(30) )
NOLOGGING;
