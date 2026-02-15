-- B34014V.ADA

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
--     CHECK THAT A DERIVED OPERATOR IS HIDDEN AND NOT FURTHER
--     DERIVABLE UNDER APPROPRIATE CIRCUMSTANCES.

--     CHECK WHEN THE DERIVED OPERATOR IS IMPLICITLY DECLARED IN THE
--     PRIVATE PART OF A PACKAGE AFTER AN EXPLICIT DECLARATION OF A
--     HOMOGRAPHIC OPERATOR IN THE VISIBLE PART.

-- HISTORY:
--     JRK 09/23/87  CREATED ORIGINAL TEST.

PROCEDURE B34014V IS

     PACKAGE P IS
          TYPE T IS RANGE -100 .. 100;
          FUNCTION "+" (X : T) RETURN T;
     END P;
     USE P;

     PACKAGE BODY P IS
          FUNCTION "+" (X : T) RETURN T IS
          BEGIN
               RETURN X + 1;
          END "+";
     END P;

BEGIN

     -----------------------------------------------------------------

     -- NEW OPERATOR DECLARED BY SUBPROGRAM DECLARATION.

     DECLARE

          PACKAGE Q IS
               TYPE QT IS PRIVATE;
               C : CONSTANT QT;
               FUNCTION "+" (Y : QT) RETURN QT;
               TYPE QR1 IS
                    RECORD
                         C1 : QT := +C;           -- OK.
                         C2 : QT := "+" (Y => C); -- OK.
                         C3 : QT := "+" (X => C); -- ERROR: + HIDDEN.
                         C4 : QT := "+" (RIGHT=>C); -- ERROR: + HIDDEN.
                    END RECORD;
          PRIVATE
               TYPE QT IS NEW T;
               C : CONSTANT QT := 0;
               TYPE QR2 IS
                    RECORD
                         C1 : QT := +0;           -- OK.
                         C2 : QT := "+" (Y => 0); -- OK.
                         C3 : QT := "+" (X => 0); -- ERROR: + HIDDEN.
                         C4 : QT := "+" (RIGHT=>0); -- ERROR: + HIDDEN.
                    END RECORD;
               TYPE QS IS NEW QT;
          END Q;
          USE Q;

          PACKAGE BODY Q IS
               FUNCTION "+" (Y : QT) RETURN QT IS
               BEGIN
                    RETURN Y + 2;
               END "+";

               PACKAGE R IS
                    Z1 : QS := +0;                -- OK.
                    Z2 : QS := "+" (Y => 0);      -- OK.
                    Z3 : QS := "+" (X => 0);      -- ERROR: + HIDDEN.
                    Z4 : QS := "+" (RIGHT => 0);  -- ERROR: + HIDDEN.
               END R;
          END Q;

          PACKAGE R IS
               Y1 : QT := +C;                     -- OK.
               Y2 : QT := "+" (Y => C);           -- OK.
               Y3 : QT := "+" (X => C);           -- ERROR: + HIDDEN.
               Y4 : QT := "+" (RIGHT => C);       -- ERROR: + HIDDEN.
               TYPE RT IS NEW QT;
               Z1 : RT := +RT(C);                 -- OK.
               Z2 : RT := "+" (Y => RT (C));      -- OK.
               Z3 : RT := "+" (X => RT (C));      -- ERROR: + HIDDEN.
               Z4 : RT := "+" (RIGHT => RT (C));  -- ERROR: + HIDDEN.
          END R;

     BEGIN
          NULL;
     END;

     -----------------------------------------------------------------

     -- NEW OPERATOR DECLARED BY RENAMING.

     DECLARE

          PACKAGE Q IS
               TYPE QT IS PRIVATE;
               C : CONSTANT QT;
               FUNCTION G (X : QT) RETURN QT;
               FUNCTION "+" (Y : QT) RETURN QT RENAMES G;
               TYPE QR1 IS
                    RECORD
                         C1 : QT := +C;           -- OK.
                         C2 : QT := "+" (Y => C); -- OK.
                         C3 : QT := "+" (X => C); -- ERROR: + HIDDEN.
                         C4 : QT := "+" (RIGHT=>C); -- ERROR: + HIDDEN.
                    END RECORD;
          PRIVATE
               TYPE QT IS NEW T;
               C : CONSTANT QT := 0;
               TYPE QR2 IS
                    RECORD
                         C1 : QT := +0;           -- OK.
                         C2 : QT := "+" (Y => 0); -- OK.
                         C3 : QT := "+" (X => 0); -- ERROR: + HIDDEN.
                         C4 : QT := "+" (RIGHT=>0); -- ERROR: + HIDDEN.
                    END RECORD;
               TYPE QS IS NEW QT;
          END Q;
          USE Q;

          PACKAGE BODY Q IS
               FUNCTION G (X : QT) RETURN QT IS
               BEGIN
                    RETURN X + 2;
               END G;

               PACKAGE R IS
                    Z1 : QS := +0;                -- OK.
                    Z2 : QS := "+" (Y => 0);      -- OK.
                    Z3 : QS := "+" (X => 0);      -- ERROR: + HIDDEN.
                    Z4 : QS := "+" (RIGHT => 0);  -- ERROR: + HIDDEN.
               END R;
          END Q;

          PACKAGE R IS
               Y1 : QT := +C;                     -- OK.
               Y2 : QT := "+" (Y => C);           -- OK.
               Y3 : QT := "+" (X => C);           -- ERROR: + HIDDEN.
               Y4 : QT := "+" (RIGHT => C);       -- ERROR: + HIDDEN.
               TYPE RT IS NEW QT;
               Z1 : RT := +RT(C);                 -- OK.
               Z2 : RT := "+" (Y => RT (C));      -- OK.
               Z3 : RT := "+" (X => RT (C));      -- ERROR: + HIDDEN.
               Z4 : RT := "+" (RIGHT => RT (C));  -- ERROR: + HIDDEN.
          END R;

     BEGIN
          NULL;
     END;

     -----------------------------------------------------------------

END B34014V;
