-- BD7204A.ADA

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
--     THE PREFIX OF THE 'POSITION, 'LAST_BIT, AND 'FIRST_BIT ATTRIBUTES
--     CANNOT DENOTE AN ARRAY COMPONENT AND CANNOT BE A NAME DECLARED BY
--     A RENAMING DECLARATION.

-- HISTORY:
--     JET 08/24/88  CREATED ORIGINAL TEST.

PROCEDURE BD7204A IS

     TYPE ARR IS ARRAY (1..10) OF INTEGER;
     A : ARR := (OTHERS => 0);

     TYPE REC_TYPE IS RECORD
          I : INTEGER;
     END RECORD;
     REC : REC_TYPE := (I => 0);

     R : INTEGER RENAMES REC.I;

     I : INTEGER;

BEGIN
     I := A(1)'POSITION;               -- ERROR: ARRAY COMPONENT.
     I := A(1)'FIRST_BIT;              -- ERROR: ARRAY COMPONENT.
     I := A(10)'LAST_BIT;              -- ERROR: ARRAY COMPONENT.

     I := R'POSITION;                  -- ERROR: RENAMED OBJECT.
     I := R'FIRST_BIT;                 -- ERROR: RENAMED OBJECT.
     I := R'LAST_BIT;                  -- ERROR: RENAMED OBJECT.
END BD7204A;
