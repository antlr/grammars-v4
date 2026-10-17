-- BC1102A.ADA

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
-- CHECK THAT GENERIC FORMAL IN OUT PARAMETER DECLARATIONS CANNOT HAVE
-- INITIALIZATIONS.

-- ASL 8/7/81
-- SPS 4/14/82

PROCEDURE BC1102A IS
 
     TYPE ENUM IS (X);
     E_CONST : CONSTANT ENUM := X;

     TYPE ARR_ENUM IS ARRAY(1..2) OF ENUM;
 
     TYPE REC_ENUM IS 
          RECORD
               COMP : ENUM;
          END RECORD;

     TASK TYPE T;
     T_OBJ : T;

     TYPE REC_TSK IS
          RECORD
               C : T;
          END RECORD;

     TYPE REC_2 IS
          RECORD
               C : REC_TSK;
          END RECORD;

     TYPE REC_2A IS
          RECORD
               D : REC_ENUM;
          END RECORD;

     TASK BODY T IS
     BEGIN
          NULL;
     END T;

     GENERIC
          F1 : IN OUT INTEGER := 5;                         -- ERROR: 
                                                   -- INITIALIZATION.
          F2 : IN OUT ENUM := E_CONST;                      -- ERROR:
                                                   -- INITIALIZATION.
          F3 : IN OUT ARR_ENUM := (1..2 => E_CONST);        -- ERROR: 
                                                   -- INITIALIZATION.
          F4 : IN OUT REC_ENUM := (COMP => E_CONST);        -- ERROR: 
                                                   -- INITIALIZATION.
          F5 : IN OUT T := T_OBJ;                           -- ERROR: 
                                                   -- INITIALIZATION.
          F6 : IN OUT REC_TSK := (C => T_OBJ);              -- ERROR: 
                                                   -- INITIALIZATION.
          F7 : IN OUT REC_2 := (C => (C => T_OBJ));         -- ERROR: 
                                                   -- INITIALIZATION.
          F8 : IN OUT REC_2A := (D => (COMP => E_CONST));   -- ERROR: 
                                                   -- INITIALIZATION.
     PACKAGE Q IS
     END Q;
BEGIN
     NULL;
END BC1102A;
