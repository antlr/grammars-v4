-- B59001F.ADA

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
--     CHECK THAT JUMPS FROM AN EXCEPTION HANDLER TO A LABEL
--     DEFINED IN THE SEQUENCE OF STATEMENTS GUARDED BY THE EXCEPTION
--     HANDLER ARE NOT ALLOWED.

--     CHECK THAT JUMPS FROM AN EXCEPTION HANDLER IN A UNIT  TO ANOTHER
--     EXCEPTION HANDLER BELONGING TO THE SAME UNIT  ARE NOT ALLOWED.

--     CHECK THAT JUMPS FROM A GUARDED SEQUENCE-OF-STATEMENTS TO AN
--     EXCEPTION HANDLER GUARDING IT  ARE NOT ALLOWED.

-- *** NOTE: This test has been modified since ACVC version 1.11 to    -- 9X
-- ***       remove incompatibilities associated with the transition   -- 9X
-- ***       to Ada 9X.                                                -- 9X


-- HISTORY:
--    RM 05/22/81 (BLOCKS ONLY)
--    RM 06/02/81 (BLOCKS, SUBPROGRAMS, PACKAGES, TASKS)
--    RM 06/04/81 (THIRD SUBOBJECTIVE ADDED)
--    SPS 3/8/83
--    DTN 03/31/93  REMOVED NUMERIC_ERROR FROM TEST.

PROCEDURE  B59001F  IS
BEGIN

     -------------------------------------------------------------------
     ------------------------  IN BLOCKS  ------------------------------

     BEGIN

          NULL ;

          DECLARE
          BEGIN
               << LAB >>
               << LAB0 >>
               GOTO L1 ;                  -- ERROR: INTO HANDLER
          EXCEPTION
               WHEN CONSTRAINT_ERROR  =>
                    << L2 >>
                    GOTO L1 ;             -- ERROR: TO FRATERNAL HANDLER
               WHEN PROGRAM_ERROR     =>
                    IF  FALSE  THEN
                         GOTO LAB ;       -- ERROR: TO GUARDED STATEMENT
                    ELSE
                         GOTO LAB0 ;      -- ERROR: TO GUARDED STATEMENT
                    END IF;
                    << L1 >>
                    GOTO L2 ;             -- ERROR: TO FRATERNAL HANDLER
          END ;

          NULL ;

     END ;


     -------------------------------------------------------------------
     ----------------------  IN SUBPROGRAMS  ---------------------------

     DECLARE

          PROCEDURE  PROC  IS
          BEGIN
               << LAB >>
               << LAB0 >>
               GOTO L2 ;                  -- ERROR: INTO HANDLER
          EXCEPTION
               WHEN CONSTRAINT_ERROR  =>
                    << L2 >>
                    GOTO L1 ;             -- ERROR: TO FRATERNAL HANDLER
               WHEN PROGRAM_ERROR     =>
                    IF  FALSE  THEN
                         GOTO LAB ;       -- ERROR: TO GUARDED STATEMENT
                    ELSE
                         GOTO LAB0 ;      -- ERROR: TO GUARDED STATEMENT
                    END IF;
                    << L1 >>
                    GOTO L2 ;             -- ERROR: TO FRATERNAL HANDLER
          END ;

     BEGIN

          NULL ;

     END ;


     DECLARE

          FUNCTION  FUNC  RETURN INTEGER  IS
          BEGIN
               << LAB >>
               << LAB0 >>
               RETURN 17 ;
               GOTO L1 ;                  -- ERROR: INTO HANDLER
          EXCEPTION
               WHEN CONSTRAINT_ERROR  =>
                    << L2 >>
                    GOTO L1 ;             -- ERROR: TO FRATERNAL HANDLER
               WHEN PROGRAM_ERROR     =>
                    IF  FALSE  THEN
                         GOTO LAB ;       -- ERROR: TO GUARDED STATEMENT
                    ELSE
                         GOTO LAB0 ;      -- ERROR: TO GUARDED STATEMENT
                    END IF;
                    << L1 >>
                    GOTO L2 ;             -- ERROR: TO FRATERNAL HANDLER
          END ;

     BEGIN

          NULL ;

     END ;


     -------------------------------------------------------------------
     -------------------------  IN PACKAGES  ---------------------------

     DECLARE

          PACKAGE  PACK  IS
               I : INTEGER ;
          END ;

          PACKAGE BODY  PACK  IS
          BEGIN
               << LAB >>
               << LAB0 >>
               GOTO L1 ;                  -- ERROR: INTO HANDLER
          EXCEPTION
               WHEN CONSTRAINT_ERROR  =>
                    << L2 >>
                    GOTO L1 ;             -- ERROR: TO FRATERNAL HANDLER
               WHEN PROGRAM_ERROR     =>
                    IF  FALSE  THEN
                         GOTO LAB ;       -- ERROR: TO GUARDED STATEMENT
                    ELSE
                         GOTO LAB0 ;      -- ERROR: TO GUARDED STATEMENT
                    END IF;
                    << L1 >>
                    GOTO L2 ;             -- ERROR: TO FRATERNAL HANDLER
          END ;

     BEGIN

          NULL ;

     END ;


     -------------------------------------------------------------------
     --------------------------  IN TASKS  -----------------------------

     DECLARE

          TASK  TK ;

          TASK BODY  TK  IS
          BEGIN
               << LAB >>
               << LAB0 >>
               GOTO L1 ;                  -- ERROR: INTO HANDLER
          EXCEPTION
               WHEN CONSTRAINT_ERROR  =>
                    << L2 >>
                    GOTO L1 ;             -- ERROR: TO FRATERNAL HANDLER
               WHEN PROGRAM_ERROR     =>
                    IF  FALSE  THEN
                         GOTO LAB ;       -- ERROR: TO GUARDED STATEMENT
                    ELSE
                         GOTO LAB0 ;      -- ERROR: TO GUARDED STATEMENT
                    END IF;
                    << L1 >>
                    GOTO L2 ;             -- ERROR: TO FRATERNAL HANDLER
          END ;

     BEGIN

          NULL ;

     END ;

     -------------------------------------------------------------------


END B59001F;
