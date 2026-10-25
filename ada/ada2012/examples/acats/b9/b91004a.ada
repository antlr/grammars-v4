-- B91004A.ADA

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
--
-- OBJECTIVE:
--     CHECK THAT THE NAME OF A TASK TYPE CANNOT BE USED AS A TYPE MARK
--     WITHIN ITS OWN BODY.
--
-- HISTORY:
--     JBG 02/24/84 CREATED THE ORIGINAL TEST.
--     EG  05/30/84
--     RDH 04/11/90 ADDED CASES FOR CONVERSIONS, MEMBERSHIP TESTS,
--                  GENERIC ACTUAL PARAMETERS, FORMAL PARAMETERS
--                  (INCLUDING GENERICS), ACCESS TYPE DECLARATIONS,
--                  ARRAY/RECORD COMPONENT DEFINITIONS, DERIVED
--                  TYPE DEFINITIONS, AND SUBTYPE DECLARATIONS.
--     LDC 09/24/90 REMOVED THE DEAD VARIABLE 'I' IN THE TASK BODY,
--                  MOVED SOME CHECKS TO 'T2' A NESTED TASK BODY,
--                  ADDED A CASE FOR USE IN A RENAME CLAUSE.

PROCEDURE B91004A IS

     TASK TYPE T IS
     END T;

     TYPE ACC_T IS ACCESS T;

     X2 : T;

     GENERIC
          TYPE J IS LIMITED PRIVATE;
     PROCEDURE P3;

     PROCEDURE P3 IS
     BEGIN
          NULL;
     END;

     PROCEDURE P4 (X : T) IS
     BEGIN
          NULL;
     END;

     TASK BODY T IS
          X : T;                   -- ERROR: USED AS TYPE MARK.
          A : ACC_T := NEW T;      -- ERROR: USED AS TYPE MARK.
          U : BOOLEAN;

          Z1 : ARRAY(1..10) OF T;  -- ERROR: USED AS ARRAY COMPONENT
                                   --        TYPE.

          TYPE Z2 IS
               RECORD
                    Z3 : T;        -- ERROR: USED AS RECORD COMPONENT
                                   --        TYPE.
               END RECORD;

          TYPE Z4 IS NEW T;        -- ERROR: USED AS PARENT TYPE.

          SUBTYPE Z5 IS T;         -- ERROR: USED AS TYPE MARK.

          PROCEDURE P IS
               X : T;              -- ERROR: USED AS TYPE MARK.
               A : ACC_T := NEW T; -- ERROR: USED AS TYPE MARK.
          BEGIN
               NULL;
          END;

          FUNCTION F1 ( Y : T) RETURN BOOLEAN IS   -- ERROR: USED AS
                                                   -- PARAMETER TYPE.
          BEGIN
               RETURN FALSE;
          END;


          PROCEDURE L IS NEW P3(J => T);     -- ERROR: USED AS ACTUAL
                                             --        PARAMETER.

          TASK TYPE T2 IS
          END T2;

          TASK BODY T2 IS

               GENERIC
                    X3 : T;             -- ERROR: USED AS PARAMETER 
                                        --        TYPE.
               PROCEDURE P5;

               TYPE A1 IS ACCESS T;     -- ERROR: USED AS DESIGNATED 
                                        --        TYPE.

               X : INTEGER RENAMES T;   -- ERROR: USED AS OBJECT NAME.

               PROCEDURE P5 IS
               BEGIN
                    NULL;
               END;

          BEGIN
               P4 (T(X2));         -- ERROR: USED IN CONVERSION.
          END T2;
     BEGIN
          P4 (T'(X2));             -- ERROR: USED AS QUALIFIER
          U := X2 IN T;            -- ERROR: USED IN MEMBERSHIP TEST.
     END;

BEGIN
     NULL;
END B91004A;
