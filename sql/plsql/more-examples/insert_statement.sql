DECLARE
   V_RECORD EVENT%ROWTYPE;
BEGIN 
   SELECT * INTO V_RECORD FROM EVENT WHERE ROWNUM=1;
   INSERT INTO TMP_EVENT VALUES V_RECORD;
END;

insert into my_table(col1, col2)
     values (1, 1);

insert into my_table(col1, col2)
     values (1, 1),
            (2, 2);
