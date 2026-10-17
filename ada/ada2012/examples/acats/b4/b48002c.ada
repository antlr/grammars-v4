-- B48002C.ADA

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
-- CHECK THAT ILLEGAL FORMS OF ALLOCATORS ARE FORBIDDEN. IN PARTICULAR,
-- FOR ALLOCATORS OF THE FORM "NEW T X", WHERE X IS A DISCRIMINANT
-- CONSTRAINT OR A VALUE OF TYPE T ENCLOSED IN PARENTHESES, CHECK THAT
-- T CANNOT BE A CONSTRAINED ACCESS SUBTYPE WHOSE DESIGNATED TYPE HAS
-- DISCRIMINANTS.

-- EG  07/26/84

PROCEDURE B48002C IS

     TYPE GENDER IS (M, F, UNKNOWN);
     SUBTYPE GENDER_TYPE IS GENDER RANGE M .. F;
     TYPE PERSON(GNDR : GENDER_TYPE := F) IS
          RECORD
               AGE : INTEGER RANGE 0 .. 100 := 0;
          END RECORD;
     TYPE PEOPLE IS ACCESS PERSON;
     SUBTYPE PEOPLE_M IS PEOPLE(M);

     TYPE A_MP IS ACCESS PEOPLE_M;

     V_A_MP : A_MP;

BEGIN

     V_A_MP := NEW PEOPLE_M(M);    -- ERROR: ALREADY CONSTRAINED.

END B48002C;
