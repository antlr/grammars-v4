-- B38008A.ADA

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
-- CHECK THAT ACCESS TYPES USED IN A SUBTYPE_INDICATION
-- CANNOT BE CONSTRAINED WITH A RANGE CONSTRAINT
-- OR ACCURACY CONSTRAINT.
-- CHECK THAT ACCESS TYPES WHICH ARE ALREADY CONSTRAINED
-- CANNOT BE FURTHER CONSTRAINED IN A SUBTYPE_INDICATION.

-- DAT 3/19/81
-- VKG 01/05/83
-- PWB 02/20/86  DELETED CODE WHICH TESTED ANOTHER OBJECTIVE.

PROCEDURE B38008A IS

     TYPE A1 IS ACCESS INTEGER RANGE 1 .. 2; -- OK.
     TYPE A2 IS ACCESS INTEGER;              -- OK.
     TYPE A3 IS NEW A2 RANGE 1 .. 2;         -- ERROR: RANGE CONSTRAINT.
     TYPE A4 IS ACCESS FLOAT DIGITS 1;       -- OK.
     TYPE A5 IS NEW A4 DIGITS 1;             -- ERROR: ACC. CONSTRAINT.
     SUBTYPE A6 IS A2 RANGE 1 .. 2;          -- ERROR: RANGE CONST.
     SUBTYPE A7 IS A4 DIGITS 1;              -- ERROR: ACC. CONSTRAINT.

     TYPE AS IS ACCESS STRING;               -- OK.
     SUBTYPE CAS IS AS (1 .. 5);             -- OK.
     SUBTYPE CAS2 IS CAS (1 .. 5);           -- ERROR: TWICE CONSTR.

     TYPE AAS IS ACCESS AS (1 .. 4);         -- OK.
     TYPE AAS2 IS ACCESS AS;                 -- OK.
     SUBTYPE AASA IS AAS (1 .. 4);           -- ERROR: TWICE CONSTR.
     SUBTYPE AASB IS AAS2 (1 .. 4);          -- ERROR: CONSTR. ACCESS.

BEGIN
     NULL;
END B38008A;
