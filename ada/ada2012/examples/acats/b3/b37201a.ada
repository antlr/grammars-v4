-- B37201A.ADA

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
--     CHECK THAT THE FORM OF A DISCRIMINANT CONSTRAINT MUST BE
--     CORRECT, NAMELY, CHECK THAT:

--     THE DISCRIMINANT_NAMES GIVEN IN THE CONSTRAINT CANNOT BE
--     DIFFERENT FROM THE NAMES OF THE DISCRIMINANTS OF THE TYPE
--     BEING CONSTRAINED.

--     THE SAME NAME CANNOT APPEAR TWICE AS A DISCRIMINANT_NAME IN A
--     PARTICULAR DISCRIMINANT_ASSOCIATION OR IN DIFFERENT DISCRIMINANT
--     ASSOCIATIONS OF THE SAME DISCRIMINANT CONSTRAINT.

--     IF A MIXTURE OF POSITIONAL AND NAMED ASSOCIATION IS USED, A
--     NAMED DISCRIMINANT ASSOCIATION CANNOT GIVE A VALUE FOR A
--     DISCRIMINANT WHOSE VALUE HAS ALREADY BEEN SPECIFIED
--     POSITIONALLY.

--     TOO MANY OR TOO FEW DISCRIMINANT VALUES CANNOT BE GIVEN.

--     POSITIONAL DISCRIMINANT VALUES CANNOT BE GIVEN AFTER A
--     DISCRIMINANT_ASSOCIATION USING DISCRIMINANT_NAMES.

--     THE BASE TYPE OF THE SPECIFIED DISCRIMINANT VALUE CANNOT BE
--     DIFFERENT FROM THE BASE TYPE OF THE CORRESPONDING DISCRIMINANT.

-- HISTORY:
--     ASL 06/16/81
--     RJW 01/16/86  RENAMED TO -B. ADDED CHECK FOR MIXED TYPES.
--     DWC 09/22/87  MOVED ONE OF THE CHECKS FOR TOO FEW VALUES
--                   TO B37201B.ADA.

PROCEDURE B37201A IS

     TYPE CONSTR1(D1 : INTEGER) IS
          RECORD
               NULL;
          END RECORD;

     TYPE CONSTR2(D2,D3 : INTEGER) IS
          RECORD
               NULL;
          END RECORD;

     TYPE CONSTR3(D4,D5,D6 : INTEGER) IS
          RECORD
               NULL;
          END RECORD;

     TYPE CONSTR4(D7 : BOOLEAN) IS
          RECORD
               NULL;
          END RECORD;

     WRONG_NAME : CONSTANT INTEGER := 1;
     T1 : CONSTR1(5);
     DD1 : INTEGER RENAMES T1.D1;

     T2 : CONSTR1(WRONG_NAME => 5);           -- ERROR: WRONG_NAME.
     T3 : CONSTR1(DD1 => 5);                  -- ERROR: RENAMED
                                              --   DISCRIMINANT.

     T4 : CONSTR2(D2|D2 => 5);                -- ERROR: SAME NAME USED
                                              --   TWICE.
     T5 : CONSTR2(D2|D2|D3 => 5);             -- ERROR: SAME NAME USED
                                              --   TWICE.
     T6 : CONSTR2(D2 => 5, D2 => 5);          -- ERROR: SAME NAME USED
                                              --   TWICE.
     T7 : CONSTR2(D2 => 5,D2 => 5,D3 => 6);   -- ERROR: SAME NAME USED
                                              --   TWICE.

     T8 : CONSTR3(0,1,D4 => 2);               -- ERROR: VALUE ALREADY
                                              --   SPECIFIED.

     T9 : CONSTR2(5,10,15);                   -- ERROR: TOO MANY VALUES.
     T10 : CONSTR2(5);                        -- ERROR: TOO FEW VALUES.

     T12 : CONSTR3(0,D5 => 1,2);              -- ERROR: POSITIONAL VALUE
                                              --   AFTER NAMED VALUE.

     T13 : CONSTR1(TRUE);                     -- ERROR: WRONG TYPE.

     TYPE NEW_INT IS NEW INTEGER RANGE 1..10;
     T14 : CONSTR2(3,3);                      -- OK.
     T15 : CONSTR2(3, NEW_INT(3));            -- ERROR: WRONG TYPE.

     TYPE MIXED(D8 : INTEGER; D9 : NEW_INT ) IS
          RECORD
               NULL;
          END RECORD;

     T16 : MIXED(D8|D9 => 3);                 -- ERROR: MIXED TYPES.

BEGIN
     NULL;
END B37201A;
