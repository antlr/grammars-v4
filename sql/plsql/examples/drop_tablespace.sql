DROP TABLESPACE tbs_01
    INCLUDING CONTENTS
        CASCADE CONSTRAINTS;
DROP TABLESPACE tbs_02
   INCLUDING CONTENTS AND DATAFILES;
DROP TABLESPACE tbs_02 IF EXISTS;