-- BD2A01H.ADA

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
--     A SIZE SPECIFICATION CANNOT BE GIVEN FOR A TYPE DECLARED BY
--     AN INCOMPLETE TYPE DECLARATION, PRIOR TO ITS FULL
--     DECLARATION.

-- HISTORY:
--     LDC  06/14/88 CREATED ORIGINAL TEST.


PROCEDURE BD2A01H IS

     TYPE TYP1;
     FOR TYP1'SIZE USE INTEGER'SIZE;                   -- ERROR: LENGTH
                                                       -- SPECIFIED
                                                       -- BEFORE FULL
                                                       -- DECLARATION.
     TYPE TYP2;
     TYPE TYP3 IS ACCESS TYP2;
     FOR TYP2'SIZE USE INTEGER'SIZE;                   -- ERROR: LENGTH
                                                       -- SPECIFIED
                                                       -- BEFORE FULL
                                                       -- DECLARATION.

     PACKAGE PCK IS
          TYPE TYP4 (DISC : INTEGER);
          FOR TYP4'SIZE USE (INTEGER'SIZE*2);          -- ERROR: LENGTH
                                                       -- SPECIFIED
                                                       -- BEFORE FULL
                                                       -- DECLARATION.
          TYPE TYP4 (DISC : INTEGER) IS RECORD
               ELEM : INTEGER;
          END RECORD;
     END PCK;

     TYPE TYP5;
     FOR TYP5'SIZE USE 4;                             -- ERROR: LENGTH
                                                       -- SPECIFIED
                                                       -- BEFORE FULL
                                                       -- DECLARATION.

-- FULL DECLARATIONS FOR THE ABOVE TYPES

     TYPE TYP1 IS NEW INTEGER;

     TYPE TYP2 IS RECORD
          ELEM : INTEGER;
     END RECORD;

     TYPE TYP5 IS (TERESA, BRIAN, PHIL, JOLEEN, LYNN, DOUG, JODIE,
                   VINCE, TOM, DAVE, JOHN, ROSA);


BEGIN
     NULL;
END BD2A01H;
