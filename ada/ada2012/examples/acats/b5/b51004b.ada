-- B51004B.ADA

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
-- CHECK THAT LABELS, LOOP IDENTIFIERS, AND BLOCK IDENTIFIERS ARE
-- IMPLICITLY DECLARED AT THE END OF THE DECLARATIVE PART.
-- FOR A NAMED BLOCK NESTED IN A NAMED LOOP, THE LOOP NAME CANNOT BE
-- USED AS A PREFIX IN AN EXPANDED BLOCK ENTITY.  SUBTESTS ARE:
--        (A) BLOCK.
--        (B) PROCEDURE BODY.
--        (C) PACKAGE BODY.
--        (D) GENERIC FUNCTION BODY.
--        (E) GENERIC PACKAGE BODY.
--        (F) TASK BODY.

-- CPP  6/4/84

PROCEDURE B51004B IS

BEGIN

     -------------------------------------------------

A :  DECLARE

     BEGIN     -- A

      L : FOR I IN 1..5 LOOP

          A1 : DECLARE
                    X : INTEGER;
                    TEMP : INTEGER;
               BEGIN     -- A1
                    TEMP := A.L.I;                     -- OK.
                    TEMP := A.L.A1.X;                  -- ERROR: 
                    TEMP := A.A1.X;                    -- OK.
                    TEMP := L.X;                       -- ERROR: 
                    TEMP := L.A1.X;                    -- ERROR: 
                    TEMP := A1.X;                      -- OK.
               END A1;

          END LOOP L;

     END A;

     -------------------------------------------------

B :  DECLARE

          PROCEDURE P (PARAM : IN OUT INTEGER) IS

          BEGIN     -- P
               
           L : LOOP

               B1 : DECLARE
                         X : INTEGER;
                         TEMP : INTEGER;
                    BEGIN     -- B1
                         TEMP := B1.X;                 -- OK.
                         TEMP := L.B1.X;               -- ERROR:  
                         TEMP := P.L.B1.X;             -- ERROR: 
                         TEMP := L.X;                  -- ERROR: 
                         TEMP := P.B1.X;               -- OK.
                         TEMP := B.L.X;                -- ERROR: 
                    END B1;

               END LOOP L;

          END P;

     BEGIN     -- B
          NULL;
     END B;

     -------------------------------------------------

C :  DECLARE

          PACKAGE PKG IS
          END PKG;

          PACKAGE BODY PKG IS
               FLAG : BOOLEAN;
          BEGIN
               FLAG := TRUE;

           L : WHILE FLAG LOOP
           
               C1 : DECLARE
                         X : INTEGER;
                         TEMP : INTEGER;
                    BEGIN     -- C1
                         TEMP := C1.X;                 -- OK.
                         TEMP := L.C1.X;               -- ERROR: 
                         TEMP := PKG.L.X;              -- ERROR: 
                         TEMP := C.PKG.C1.X;           -- OK.
                         TEMP := C.L.C1.X;             -- ERROR: 
                    END C1;

                    FLAG := FALSE;
               END LOOP L;

          END PKG;

     BEGIN     -- C
          NULL;
     END C;

     ---------------------------------------------------

     D :  DECLARE

               GENERIC
                    TYPE Q IS (<>);
               FUNCTION FN RETURN INTEGER;

               FUNCTION FN RETURN INTEGER IS
                    FLAG : BOOLEAN;
                    TEMP : INTEGER;
               BEGIN     
                    FLAG := TRUE;

                L : WHILE FLAG LOOP

                    D1 : DECLARE
                              X : INTEGER;
                         BEGIN     -- D1
                              TEMP := FN.L.D1.X;       -- ERROR:
                              TEMP := FN.L.X;          -- ERROR:
                              TEMP := D1.X;            -- OK.
                              TEMP := L.D1.X;          -- ERROR:
                              TEMP := L.X;             -- ERROR:
                              TEMP := D.FN.D1.X;       -- OK.
                         END D1;
                         
                         FLAG := FALSE;
                    END LOOP L;

                    RETURN TEMP;
               END FN;

          BEGIN     -- D
               NULL;
          END D;

     -------------------------------------------------

E :  DECLARE

          GENERIC
               TYPE ELEMENT IS (<>);
               ITEM : ELEMENT;
          PACKAGE PKG IS
          END PKG;

          PACKAGE BODY PKG IS
          BEGIN

          L :  FOR I IN 1..5 LOOP

               E1 : DECLARE
                         X : INTEGER;
                         TEMP : INTEGER;
                    BEGIN     -- E1
                         TEMP := E1.X;                 -- OK.
                         TEMP := L.E1.X;               -- ERROR: 
                         TEMP := L.I;                  -- OK.
                         TEMP := PKG.L.I;              -- OK.
                         TEMP := E.PKG.L.E1.X;         -- ERROR: 
                         TEMP := PKG.L.X;              -- ERROR: 
                    END E1;

               END LOOP L;

          END PKG;

     BEGIN     -- E
          NULL;
     END E;

     -------------------------------------------------

F :  DECLARE

          TASK T;

          TASK BODY T IS
          BEGIN

          L :  LOOP

               F1 : DECLARE
                         X : INTEGER;
                         TEMP : INTEGER;
                    BEGIN     -- F1
                         TEMP := F1.X;                 -- OK.
                         TEMP := L.F1.X;               -- ERROR: 
                         TEMP := T.L.F1.X;             -- ERROR: 
                         TEMP := T.F1.X;               -- OK.
                         TEMP := F.L.X;                -- ERROR: 
                         TEMP := L.X;                  -- ERROR: 
                    END F1;

               END LOOP L;

          END T;

     BEGIN     -- F
          NULL;
     END F;

     -------------------------------------------------

END B51004B;
