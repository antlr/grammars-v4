-- B85004A.ADA

--                             Grant of Unlimited Rights
--
--     Under contracts F33600-87-D-0337, F33600-84-D-0280, MDA903-79-C-0687,
--     F08630-91-C-0015, and DCA100-97-D-0025, the U.S. Government obtained 
--     unlimited rights in the software and documentation contained herein.
--     Unlimited rights are defined in DFAR 252.227-7013(a)(19).  By making 
--     this public release, the Government intends to confer upon all 
--     recipients unlimited rights  equal to those held by the Government.  
--     These rights include rights to use, duplicate, release or disclose the 
--     released technical data and computer software in whole or in part, in 
--     any manner and for any purpose whatsoever, and to have or permit others 
--     to do so.
--
--                                    DISCLAIMER
--
--     ALL MATERIALS OR INFORMATION HEREIN RELEASED, MADE AVAILABLE OR
--     DISCLOSED ARE AS IS.  THE GOVERNMENT MAKES NO EXPRESS OR IMPLIED 
--     WARRANTY AS TO ANY MATTER WHATSOEVER, INCLUDING THE CONDITIONS OF THE
--     SOFTWARE, DOCUMENTATION OR OTHER INFORMATION RELEASED, MADE AVAILABLE 
--     OR DISCLOSED, OR THE OWNERSHIP, MERCHANTABILITY, OR FITNESS FOR A
--     PARTICULAR PURPOSE OF SAID MATERIAL.
--*
-- OBJECTIVE:
--     CHECK THAT A RENAMED CONSTANT OBJECT, "IN" PARAMETER OF A
--     SUBPROGRAM OR ENTRY, "IN" FORMAL GENERIC, RECORD DISCRIMINANT,
--     LOOP PARAMETER, DEFERRED CONSTANT, OR RENAMED CONSTANT IS STILL
--     CONSIDERED A CONSTANT.

-- HISTORY:
--     JET 03/14/88  CREATED ORIGINAL TEST.

PROCEDURE B85004A IS

     TYPE A IS ARRAY (POSITIVE RANGE <>) OF INTEGER;

     C1 : CONSTANT INTEGER := 1;
     X1 : INTEGER RENAMES C1;
     X2 : INTEGER RENAMES X1;

     TYPE REC (D : POSITIVE := 1) IS
          RECORD
               I : A(1..D);
          END RECORD;
     TYPE ACCREC1 IS ACCESS REC;
     TYPE ACCREC2 IS ACCESS REC(10);

     R1 : REC;
     R2 : REC(10);
     AR1 : ACCREC1 := NEW REC;
     AR2 : ACCREC2 := NEW REC(10);

     X3 : POSITIVE RENAMES R1.D;
     X4 : POSITIVE RENAMES R2.D;
     X5 : POSITIVE RENAMES AR1.D;
     X6 : POSITIVE RENAMES AR2.D;

     C2 : CONSTANT A(1..3) := (1, 2, 3);
     X7 : INTEGER RENAMES C2(1);

     GENERIC
          K1 : IN INTEGER;
     PACKAGE GENPKG IS
          TYPE K IS PRIVATE;
          K2 : CONSTANT K;
     PRIVATE
          TYPE K IS RANGE 1..100;
          K2 : CONSTANT K := 1;
     END GENPKG;

     PACKAGE PKG IS NEW GENPKG(10);

     TASK FOOEY IS
          ENTRY ENT1 (I : IN INTEGER);
     END FOOEY;

     PROCEDURE TESTPROC (I : IN OUT INTEGER) IS
     BEGIN
          I := 1;
     END TESTPROC;

     GENERIC
          X : IN OUT INTEGER;
     PACKAGE TESTPACK IS
     END TESTPACK;

     PACKAGE BODY TESTPACK IS
     BEGIN
          X := 1;
     END TESTPACK;

     TASK BODY FOOEY IS
     BEGIN
          ACCEPT ENT1 (I : IN INTEGER) DO
               DECLARE
                    TX1 : INTEGER RENAMES I;
                    PACKAGE PKGTX1 IS NEW TESTPACK(TX1); -- ERROR:
                                                 -- RENAMED 'IN' PARM.
               BEGIN
                    TX1 := 2;                    -- ERROR:
                                                 --  RENAMED 'IN' PARM.
                    TESTPROC(TX1);               -- ERROR:
                                                 --  RENAMED 'IN' PARM.
               END;
          END ENT1;
     END FOOEY;

     PACKAGE BODY GENPKG IS
          KX1 : INTEGER RENAMES K1;
          PACKAGE PKGKX1 IS NEW TESTPACK(KX1);   -- ERROR:
                                                 --  RENAMED GENERIC
                                                 --  FORMAL PARAMETER.
          KX2 : K RENAMES K2;
     BEGIN
          KX1 := 2;                              -- ERROR:
                                                 --  RENAMED GENERIC
                                                 --  FORMAL PARAMTER.
          TESTPROC(KX1);                         -- ERROR:
                                                 --  RENAMED GENERIC
                                                 --  FORMAL PARAMTER.
          KX2 := K'(2);                          -- ERROR:
                                                 --  RENAMED
                                                 --  DEFERRED CONSTANT.
     END GENPKG;

     PROCEDURE PROC (I : IN INTEGER) IS
          PX1 : INTEGER RENAMES I;
          PACKAGE PKGPX1 IS NEW TESTPACK(PX1);   -- ERROR:
                                                 --  RENAMED 'IN'
                                                 --  PARAMTER.
     BEGIN
          PX1 := 2;                              -- ERROR:
                                                 --  RENAMED 'IN'
                                                 --  PARAMTER.
          TESTPROC(PX1);                         -- ERROR:
                                                 --  RENAMED 'IN'
                                                 --  PARAMTER.
     END PROC;

     PACKAGE PKGX1 IS NEW TESTPACK(X1);          -- ERROR:
                                                 --  RENAMED CONSTANT.
     PACKAGE PKGX2 IS NEW TESTPACK(X2);          -- ERROR:
                                                 --  RENAMED RENAMED
                                                 --  CONSTANT.
     PACKAGE PKGX3 IS NEW TESTPACK(X3);          -- ERROR:
                                                 --  RENAMED
                                                 --  DISCRIMINANT.
     PACKAGE PKGX4 IS NEW TESTPACK(X4);          -- ERROR:
                                                 --  RENAMED
                                                 --  DISCRIMINANT.
     PACKAGE PKGX5 IS NEW TESTPACK(X5);          -- ERROR:
                                                 --  RENAMED
                                                 --  DISCRIMINANT.
     PACKAGE PKGX6 IS NEW TESTPACK(X6);          -- ERROR:
                                                 --  RENAMED
                                                 --  DISCRIMINANT.
     PACKAGE PKGX7 IS NEW TESTPACK(X7);          -- ERROR:
                                                 --  RENAMED CONSTANT
                                                 --  ARRAY ELEMENT.

BEGIN
     X1 := 2;                                    -- ERROR:
                                                 --  RENAMED CONSTANT.
     TESTPROC(X1);                               -- ERROR:
                                                 --  RENAMED CONSTANT.
     X2 := 2;                                    -- ERROR:
                                                 --  RENAMED RENAMED
                                                 --  CONSTANT.
     TESTPROC(X2);                               -- ERROR:
                                                 --  RENAMED RENAMED
                                                 --  CONSTANT.
     X3 := 2;                                    -- ERROR:
                                                 --  RENAMED
                                                 --  DISCRIMINANT.
     TESTPROC(X3);                               -- ERROR:
                                                 --  RENAMED
                                                 --  DISCRIMINANT.
     X4 := 2;                                    -- ERROR:
                                                 --  RENAMED
                                                 --  DISCRIMINANT.
     TESTPROC(X4);                               -- ERROR:
                                                 --  RENAMED
                                                 --  DISCRIMINANT.
     X5 := 2;                                    -- ERROR:
                                                 --  RENAMED
                                                 --  DISCRIMINANT.
     TESTPROC(X5);                               -- ERROR:
                                                 --  RENAMED
                                                 --  DISCRIMINANT.
     X6 := 2;                                    -- ERROR:
                                                 --  RENAMED
                                                 --  DISCRIMINANT.
     TESTPROC(X6);                               -- ERROR:
                                                 --  RENAMED
                                                 --  DISCRIMINANT.
     X7 := 2;                                    -- ERROR:
                                                 --  RENAMED CONSTANT
                                                 --  ARRAY ELEMENT.
     TESTPROC(X7);                               -- ERROR:
                                                 --  RENAMED CONSTANT
                                                 --  ARRAY ELEMENT.

     FOR I IN 1..10 LOOP
          DECLARE
               X8 : INTEGER RENAMES I;
               PACKAGE PKGX8 IS NEW TESTPACK(X8); -- ERROR:
                                                 --  RENAMED LOOP
                                                 --  COUNTER.
          BEGIN
               X8 := 2;                          -- ERROR:
                                                 --  RENAMED LOOP
                                                 --  COUNTER.
               TESTPROC(X8);                     -- ERROR:
                                                 --  RENAMED LOOP
                                                 --  COUNTER.
          END;
     END LOOP;

END B85004A;
