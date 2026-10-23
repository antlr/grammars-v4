-- B34014M.ADA

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
--     CHECK THAT A DERIVED SUBPROGRAM IS HIDDEN AND NOT FURTHER
--     DERIVABLE UNDER APPROPRIATE CIRCUMSTANCES.

--     CHECK WHEN THE DERIVED SUBPROGRAM IS IMPLICITLY DECLARED IN A
--     DECLARATIVE PART AND A HOMOGRAPHIC SUBPROGRAM IS LATER DECLARED
--     EXPLICITLY IN THE SAME DECLARATIVE PART.

-- HISTORY:
--     JRK 09/17/87  CREATED ORIGINAL TEST.
--     PWN 12/01/95  REMOVED CHECKS WHERE VISIBILITY RULES HAVE CHANGED.
--     PWN 02/01/96  RESTORED CHECKS IN ADA 95 LEGAL FORMAT.

PROCEDURE B34014M IS

     PACKAGE P IS
          TYPE T IS RANGE -100 .. 100;
          FUNCTION F (X : T) RETURN T;
     END P;
     USE P;

     PACKAGE BODY P IS
          FUNCTION F (X : T) RETURN T IS
          BEGIN
               RETURN 1;
          END F;
     END P;

BEGIN

     -----------------------------------------------------------------

     -- NEW SUBPROGRAM DECLARED BY SUBPROGRAM DECLARATION IN A BLOCK.

     BEGIN

      Q : DECLARE
               TYPE QT IS NEW T;
               X : QT := F (0);                   -- OK.
               FUNCTION F (Y : QT) RETURN QT;
               TYPE QR IS
                    RECORD
                         C1 : QT := F (0);        -- OK.
                         C2 : QT := F (Y => 0);   -- OK.
                         C3 : QT := F (X => 0);   -- ERROR: F HIDDEN.
                    END RECORD;
               TYPE QS IS NEW QT;

               FUNCTION F (Y : QT) RETURN QT IS
               BEGIN
                    RETURN 2;
               END F;

               PACKAGE R IS
                    Z1 : QS := F (0);             -- OK.
                    Z2 : QS := F (X => 0);        -- ERROR: F HIDDEN.
                    Z3 : QS := F (Y => 0);        -- OK.
               END R;
          BEGIN
               NULL;
          END Q;

     END;

     -----------------------------------------------------------------

     -- NEW SUBPROGRAM DECLARED BY RENAMING IN A PROCEDURE BODY.

     DECLARE

          PROCEDURE Q IS
               TYPE QT IS NEW T;
               X : QT := F (0);                   -- OK.
               FUNCTION G (X : QT) RETURN QT;
               FUNCTION F (Y : QT) RETURN QT RENAMES G;
               TYPE QR IS
                    RECORD
                         C1 : QT := F (0);        -- OK.
                         C2 : QT := F (Y => 0);   -- OK.
                         C3 : QT := F (X => 0);   -- ERROR: F HIDDEN.
                    END RECORD;
               TYPE QS IS NEW QT;

               FUNCTION G (X : QT) RETURN QT IS
               BEGIN
                    RETURN 2;
               END G;

               PACKAGE R IS
                    Z1 : QS := F (0);             -- OK.
                    Z2 : QS := F (X => 0);        -- ERROR: F HIDDEN.
                    Z3 : QS := F (Y => 0);        -- OK.
               END R;
          BEGIN
               NULL;
          END Q;

     BEGIN
          NULL;
     END;

     -----------------------------------------------------------------

     -- NEW SUBPROGRAM DECLARED BY GENERIC INSTANTIATION IN A PACKAGE
     -- BODY.

     DECLARE

          GENERIC
               TYPE T IS RANGE <>;
          FUNCTION G (Y : T) RETURN T;

          FUNCTION G (Y : T) RETURN T IS
          BEGIN
               RETURN 2;
          END G;

          PACKAGE Q IS
          END Q;

          PACKAGE BODY Q IS
               TYPE QT IS NEW T;
               X : QT := F (0);                   -- OK.
               FUNCTION F IS NEW G (QT);
               W1 : QT := F (0);                  -- OK.
               W2 : QT := F (Y => 0);             -- OK.
               W3 : QT := F (X => 0);             -- ERROR: F HIDDEN.
               TYPE QS IS NEW QT;
               Z1 : QS := F (0);                  -- OK.
               Z2 : QS := F (X => 0);             -- ERROR: F HIDDEN.
               Z3 : QS := F (Y => 0);             -- OK.
          END Q;

     BEGIN
          NULL;
     END;

     -----------------------------------------------------------------

     -- NEW SUBPROGRAM DECLARED BY SUBPROGRAM DECLARATION IN A TASK
     -- BODY.

     DECLARE

          TASK Q;

          TASK BODY Q IS
               TYPE QT IS NEW T;
               X : QT := F (0);                   -- OK.
               FUNCTION F (Y : QT) RETURN QT;
               TYPE QR IS
                    RECORD
                         C1 : QT := F (0);        -- OK.
                         C2 : QT := F (Y => 0);   -- OK.
                         C3 : QT := F (X => 0);   -- ERROR: F HIDDEN.
                    END RECORD;
               TYPE QS IS NEW QT;

               FUNCTION F (Y : QT) RETURN QT IS
               BEGIN
                    RETURN 2;
               END F;

               PACKAGE R IS
                    Z1 : QS := F (0);             -- OK.
                    Z2 : QS := F (X => 0);        -- ERROR: F HIDDEN.
                    Z3 : QS := F (Y => 0);        -- OK.
               END R;
          BEGIN
               NULL;
          END Q;

     BEGIN
          NULL;
     END;

     -----------------------------------------------------------------

     -- NEW SUBPROGRAM DECLARED BY RENAMING IN A GENERIC PACKAGE BODY.

     DECLARE

          GENERIC
          PACKAGE Q IS
          END Q;

          PACKAGE BODY Q IS
               TYPE QT IS NEW T;
               X : QT := F (0);                   -- OK.
               FUNCTION G (X : QT) RETURN QT;
               FUNCTION F (Y : QT) RETURN QT RENAMES G;
               TYPE QR IS
                    RECORD
                         C1 : QT := F (0);        -- OK.
                         C2 : QT := F (Y => 0);   -- OK.
                         C3 : QT := F (X => 0);   -- ERROR: F HIDDEN.
                    END RECORD;
               TYPE QS IS NEW QT;

               FUNCTION G (X : QT) RETURN QT IS
               BEGIN
                    RETURN 2;
               END G;

               PACKAGE R IS
                    Z1 : QS := F (0);             -- OK.
                    Z2 : QS := F (X => 0);        -- ERROR: F HIDDEN.
                    Z3 : QS := F (Y => 0);        -- OK.
               END R;
          END Q;

     BEGIN
          NULL;
     END;

     -----------------------------------------------------------------

     -- NEW SUBPROGRAM DECLARED BY GENERIC INSTANTIATION IN A GENERIC
     -- FUNCTION BODY.

     DECLARE

          GENERIC
               TYPE T IS RANGE <>;
          FUNCTION G (Y : T) RETURN T;

          FUNCTION G (Y : T) RETURN T IS
          BEGIN
               RETURN 2;
          END G;

          GENERIC
               TYPE S IS RANGE <>;
          FUNCTION Q (I : S) RETURN S;

          FUNCTION Q (I : S) RETURN S IS
               TYPE QT IS NEW T;
               X : QT := F (0);                   -- OK.
               FUNCTION F IS NEW G (QT);
               W1 : QT := F (0);                  -- OK.
               W2 : QT := F (Y => 0);             -- OK.
               W3 : QT := F (X => 0);             -- ERROR: F HIDDEN.
               TYPE QS IS NEW QT;
               Z1 : QS := F (0);                  -- OK.
               Z2 : QS := F (X => 0);             -- ERROR: F HIDDEN.
               Z3 : QS := F (Y => 0);             -- OK.
          BEGIN
               RETURN I;
          END Q;

     BEGIN
          NULL;
     END;

     -----------------------------------------------------------------

END B34014M;
