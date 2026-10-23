-- B51004C.ADA

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
--    IMPLICITLY DECLARED AT THE END OF THE DECLARATIVE PART.
--    IDENTIFIERS DECLARED AS VARIABLES IN OUTER SCOPES ARE HENCE 
--    HIDDEN.  SUBTESTS ARE:
--        (A)  BLOCK.
--        (B)  PROCEDURE BODY.
--        (C)  PACKAGE BODY.
--        (D)  GENERIC FUNCTION BODY.
--        (E)  GENERIC PACKAGE BODY.
--        (F)  TASK BODY.

-- CPP  5/30/84

PROCEDURE B51004C IS

BEGIN

OUTER: DECLARE

          IDN1, IDN2, IDN3 : INTEGER;

     ---------------------------------------------------

     BEGIN     -- OUTER

     A :  DECLARE

               TEMP : INTEGER;

          BEGIN     -- A
               
                <<IDN1>> TEMP := 0;

                         IDN1 := 1;          -- ERROR: LABEL IDN1
                                             -- IN SCOPE.

                         IDN2 := 2;          -- ERROR: LABEL IDN2
                                             -- IN SCOPE.

                  IDN2 : FOR I IN 1..5 LOOP
                              TEMP := 0;
                         END LOOP IDN2;

                  IDN3 : BEGIN
                              IDN3 := 3;     -- ERROR: LABEL IDN3
                         END IDN3;           -- IN SCOPE.
          
          END A;

     ---------------------------------------------------

     B :  DECLARE

               PROCEDURE P (TEMP : IN OUT INTEGER) IS
               BEGIN

                <<IDN1>> TEMP := 0;
                         
                         IDN1 := 1;          -- ERROR: LABEL IDN1
                                             -- IN SCOPE.

                  IDN2 : WHILE TEMP > 0 LOOP
                              IDN2 := 2;     -- ERROR: LABEL IDN2
                         END LOOP IDN2;      -- IN SCOPE.

                         IDN3 := 3;          -- ERROR: LABEL IDN3
                                             -- IN SCOPE.
                  IDN3 : DECLARE
                         BEGIN
                              TEMP := 0;
                         END IDN3;

               END P;

          BEGIN     -- B
               NULL;
          END B;

     ---------------------------------------------------

     C :  DECLARE

               PACKAGE PKG IS
               END PKG;

               PACKAGE BODY PKG IS

                    TEMP : INTEGER;

               BEGIN

                       IDN1 := 1;               -- ERROR: LABEL IDN1 
                                                -- IN SCOPE.
             <<IDN1>>  TEMP := 0;

                       IDN2 := 2;               -- ERROR: LABEL IDN2
                                                -- IN SCOPE.
                IDN2 : LOOP
                            TEMP := 0;
                       END LOOP IDN2;

                IDN3 : BEGIN
                            IDN3 := 3;          -- ERROR: LABEL IDN3
                       END IDN3;                -- IN SCOPE.

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

                    TEMP : INTEGER;

               BEGIN     

                         IDN1 := 1;          -- ERROR: LABEL IDN1
                                             -- IN SCOPE.
                <<IDN1>> TEMP := 0;

                         IDN2 := 2;          -- ERROR: LABEL IDN2
                                             -- IN SCOPE.
                  IDN2 : FOR I IN 1..5 LOOP
                              TEMP := 0;
                         END LOOP IDN2;

                  IDN3 : BEGIN
                              IDN3 := 3;     -- ERROR: LABEL IDN3
                         END IDN3;           -- IN SCOPE.
                    
                         RETURN TEMP;

               END FN;

          BEGIN
               NULL;
          END D;

     ---------------------------------------------------
     E :  DECLARE

               GENERIC

                    TYPE ELEMENT IS PRIVATE;
                    ITEM : ELEMENT;

               PACKAGE PKG IS
               END PKG;

               PACKAGE BODY PKG IS
                    
                    TEMP : ELEMENT;

               BEGIN

                <<IDN1>> TEMP := ITEM;
                         IDN1 := 1;               -- ERROR: LABEL IDN1
                                                  -- IN SCOPE.

                  IDN2 : WHILE TEMP /= ITEM LOOP    
                              IDN2 := 2;          -- ERROR: LABEL IDN2
                         END LOOP IDN2;           -- IN SCOPE.         

                         IDN3 := 3;               -- ERROR: LABEL IDN3
                                                  -- IN SCOPE.
                  IDN3 : DECLARE
                         BEGIN
                              TEMP := ITEM;
                         END IDN3;

               END PKG;

          BEGIN     -- E
               NULL;
          END E;

     ---------------------------------------------------

     F :  DECLARE

               TASK T IS
                    ENTRY DO_THIS (TEMP : IN OUT INTEGER);
               END T;

               TASK BODY T IS

               BEGIN

                    LOOP
                         SELECT
                              ACCEPT DO_THIS (TEMP: IN OUT INTEGER) DO

                                        IDN1 := 1;     -- ERROR: IDN1.

                               <<IDN1>> TEMP := 0;
                                   
                                 IDN2 : LOOP
                                            IDN2 := 2; -- ERROR: IDN2.
                                        END LOOP IDN2;

                                        IDN3 := 3;     -- ERROR: IDN3.
                                 IDN3 : DECLARE
                                        BEGIN
                                             TEMP := 0;
                                        END IDN3;

                              END DO_THIS;
                         END SELECT;
                    END LOOP;

               END T;

          BEGIN     -- F
               NULL;
          END F;

     ---------------------------------------------------

     END OUTER;

END B51004C;
