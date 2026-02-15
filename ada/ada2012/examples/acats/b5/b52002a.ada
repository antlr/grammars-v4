-- B52002A.ADA

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
-- CHECK THAT THE LEFT SIDE OF AN ASSIGNMENT STMT MUST BE A VARIABLE.

-- "SEMANTICS" VIOLATIONS (CONTEXT-SENSITIVE RESTRICTIONS).

-- CHECK THAT THE LHS MAY NOT BE A FORM OF A CONSTANT.

-- DCB 1/31/80
-- JRK 10/30/80
-- RM  06/09/82  (REVISION FOR ADA-82)
-- RM  07/13/82  (SEPARATE "SYNTAX" FROM "SEMANTICS" (NON-CF SYNTAX))
-- SPS 03/21/83

PROCEDURE B52002A IS

     C1 : CONSTANT := 3;
     C2 : CONSTANT INTEGER := 4;
     CA : CONSTANT ARRAY (1..3) OF INTEGER := (1,2,3);

     TYPE DRT (I : INTEGER := 0) IS
          RECORD
               B : BOOLEAN;
          END RECORD;
     DR : DRT;

     EX : INTEGER ;

     TYPE T IS ACCESS INTEGER;

     FUNCTION F (I : INTEGER) RETURN T IS
     BEGIN
          I := 3;       -- ERROR: IN PARAMETER ON LEFT.
          RETURN NEW INTEGER'(5);
     END F;

BEGIN

     C1 := 6;           -- ERROR: CONSTANT ON LEFT.
     C2 := 7;           -- ERROR: CONSTANT ON LEFT.
     CA(2) := 9;        -- ERROR: CONSTANT COMPONENT ON LEFT.

     FOR I IN 1..10 LOOP
          I := I + 1;   -- ERROR: LOOP PARAMETER ON LEFT.
     END LOOP;

     DR.I := 4;         -- ERROR: DISCRIMINANT COMPONENT ON LEFT.

END B52002A;
