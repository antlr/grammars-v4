-- B46004E.ADA

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
-- WHEN THE TARGET TYPE OF A TYPE CONVERSION IS AN ARRAY TYPE, CHECK 
-- THAT:
--     (A). IF THE COMPONENT SUBTYPE FOR THE OPERAND AND THE TARGET 
--          TYPE IS A RECORD OR PRIVATE TYPE WITH DISCRIMINANTS, THE 
--          COMPONENT SUBTYPE OF THE TARGET CANNOT BE CONSTRAINED IF 
--          THE COMPONENT SUBTYPE OF THE OPERAND IS NOT CONSTRAINED OR
--          VICE VERSA.
--     (B). IF THE COMPONENT SUBTYPE OF THE OPERAND AND THE TARGET TYPE
--          IS AN ACCESS TYPE WHOSE DESIGNATED TYPE IS AN ARRAY TYPE 
--          OR A TYPE WITH DISCRIMINANTS, ONE COMPONENT SUBTYPE CANNOT 
--          BE CONSTRAINED UNLESS THE OTHER IS ALSO CONSTRAINED.
                    
-- R.WILLIAMS 9/5/86

PROCEDURE B46004E IS

     TYPE REC (D : INTEGER := 3) IS
          RECORD                   
               NULL;
          END RECORD;

     TYPE RECARRC IS ARRAY (1 .. 2) OF REC (3);
     RC : RECARRC := (1 .. 2 => (D => 3));

     TYPE RECARRU IS ARRAY (1 .. 2) OF REC;
     RU : RECARRU := (1 .. 2 => (D => 3));

     TYPE RACC IS ACCESS REC;
     A : RACC := NEW REC (D => 3);

     TYPE RACARRC IS ARRAY (1 .. 2) OF RACC (3);
     AC : RACARRC := (1 .. 2 => A);

     TYPE RACARRU IS ARRAY (1 .. 2) OF RACC;
     AU : RACARRU := (1 .. 2 => A);

     TYPE ARR IS ARRAY (NATURAL RANGE <>) OF INTEGER;
     
     TYPE ACCA IS ACCESS ARR;
     AR : ACCA := NEW ARR'(1 .. 3 => 0);

     TYPE ACARRC IS ARRAY (1 .. 2) OF ACCA (1 .. 3);
     ACC : ACARRC := (1 .. 2 => AR);

     TYPE ACARRU IS ARRAY (1 .. 2) OF ACCA;
     ACU : ACARRU := (1 .. 2 => AR);

     PACKAGE PKG1 IS

          TYPE PRIV (D : INTEGER := 3) IS PRIVATE;

          TYPE PRARRC IS ARRAY (1 .. 2) OF PRIV (3);
          TYPE PRARRU IS ARRAY (1 .. 2) OF PRIV;
          
          TYPE PACC IS ACCESS PRIV;

          TYPE PACARRC IS ARRAY (1 .. 2) OF PACC (3);
          TYPE PACARRU IS ARRAY (1 .. 2) OF PACC;

     PRIVATE 
          TYPE PRIV (D : INTEGER := 3) IS
               RECORD
                    NULL;
               END RECORD;
     END PKG1;

     USE PKG1;

     P1  : PRIV (D => 3);
     PRC : PRARRC := (1 .. 2 => P1);
     PRU : PRARRU := (1 .. 2 => P1);

     P2 : PACC := NEW PRIV (D => 3);

     PAC : PACARRC := (1 .. 2 => P2);
     PAU : PACARRU := (1 .. 2 => P2);

          
BEGIN
     RC := RECARRC (RU);                 -- ERROR: (A).
     RU := RECARRU (RC);                 -- ERROR: (A).

     AC := RACARRC (AU);                 -- ERROR: (B).
     AU := RACARRU (AC);                 -- ERROR: (B).

     ACC := ACARRC (ACU);                -- ERROR: (B).
     ACU := ACARRU (ACC);                -- ERROR: (B).

     PRC := PRARRC (PRU);                -- ERROR: (A).
     PRU := PRARRU (PRC);                -- ERROR: (A).

     PAC := PACARRC (PAU);               -- ERROR: (B).
     PAU := PACARRU (PAC);               -- ERROR: (B).


END B46004E;
