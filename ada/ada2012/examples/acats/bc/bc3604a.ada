-- BC3604A.ADA

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
--     CHECK THAT GENERIC FORMAL AND ACTUAL SUBPROGRAMS MUST HAVE THE
--     SAME PARAMETER/RESULT PROFILE AND THAT CORRESPONDING PARAMETER
--     MODES MUST BE THE SAME.

-- HISTORY:
--     PWB  03/06/86 CREATED ORIGINAL TEST.
--     DHH 10/19/87 SHORTENED LINES CONTAINING MORE THAN 72 CHARACTERS.

PROCEDURE BC3604A IS

     GENERIC
          WITH PROCEDURE FORMAL_PROC (P1 : IN     INTEGER;
                                      P2 : OUT    CHARACTER;
                                      P3 : IN OUT BOOLEAN);
     PACKAGE GEN_1 IS
     END GEN_1;

     GENERIC
          WITH FUNCTION FORMAL_FUNC (P1 : INTEGER) RETURN BOOLEAN;
     PACKAGE GEN_2 IS
     END GEN_2;

     GENERIC
          WITH PROCEDURE FORMAL_PROC (P1 : INTEGER);
     PACKAGE GEN_3 IS
     END GEN_3;

     GENERIC
          WITH PROCEDURE FORMAL_PROC (P1 : OUT    INTEGER;
                                      P2 : IN     CHARACTER := 'A';
                                      P3 : IN  BOOLEAN := TRUE);
     PACKAGE GEN_4 IS
     END GEN_4;

     GENERIC
          TYPE FORMAL_TYPE IS PRIVATE;
          WITH FUNCTION FORMAL_FUNC (P : FORMAL_TYPE)
                                                    RETURN FORMAL_TYPE;
     PACKAGE GEN_5 IS
     END GEN_5;

     GENERIC
          TYPE FORMAL_TYPE IS PRIVATE;
          WITH PROCEDURE FORMAL_PROC (P : FORMAL_TYPE);
     PACKAGE GEN_6 IS
     END GEN_6;

BEGIN   -- BC3604A

     DECLARE     -- A) PROCEDURE VS. FUNCTION

          FUNCTION ACTUAL_FUN (P : INTEGER)
                   RETURN BOOLEAN IS
          BEGIN
               RETURN TRUE;
          END ACTUAL_FUN;

          PROCEDURE ACTUAL_PROC (P : INTEGER) IS
          BEGIN
               NULL;
          END ACTUAL_PROC;

          PACKAGE INST_WITH_FUN IS
                  NEW GEN_3 (ACTUAL_FUN);             -- ERROR:
                                                      -- PROC REQUIRED.
          PACKAGE INST_WITH_PROC IS
                  NEW GEN_2 (ACTUAL_PROC);            -- ERROR:
                                                      -- FUNCTION REQ'D.
     BEGIN      -- A)
          NULL;
     END;       -- A)

---------------------------------------------------------------------

     DECLARE    -- B) BASE TYPE OF ONE PARAMETER DIFFERENT

          TYPE LOGICAL IS NEW BOOLEAN;
          PROCEDURE ACTUAL_PROC (P1 : IN     INTEGER;
                                 P2 : OUT    CHARACTER;
                                 P3 : IN OUT LOGICAL) IS
          BEGIN
               NULL;
          END ACTUAL_PROC;

          PROCEDURE ONE_PARM_PROC (P : LOGICAL) IS
          BEGIN
               NULL;
          END ONE_PARM_PROC;

          FUNCTION ACTUAL_FUNC (P : CHARACTER)
                               RETURN BOOLEAN IS
          BEGIN
               RETURN TRUE;
          END ACTUAL_FUNC;

          FUNCTION LOGICAL_ACTUAL_FUNC (P : INTEGER)
                                   RETURN LOGICAL IS
          BEGIN
               RETURN TRUE;
          END LOGICAL_ACTUAL_FUNC;

          PACKAGE INST_WITH_WRONG_P3 IS
                  NEW GEN_1 (ACTUAL_PROC);            -- ERROR: 3RD PARM
                                                      -- WRONG TYPE.
          PACKAGE INST_WITH_WRONG_P1 IS
                  NEW GEN_2 (ACTUAL_FUNC);            -- ERROR: PARM HAS
                                                      -- WRONG TYPE.
          PACKAGE INST_WITH_WRONG_RES IS
                  NEW GEN_2 (LOGICAL_ACTUAL_FUNC);    -- ERROR: RESULT
                                                      -- HAS WRONG TYPE.
          PACKAGE INST_BAD_FORMAL_RES IS
                  NEW GEN_5 (CHARACTER, ACTUAL_FUNC); -- ERROR: RESULT
                                                      -- HAS WRONG TYPE.
          PACKAGE INST_BAD_FORMAL_PARM IS
                  NEW GEN_5 (BOOLEAN, ACTUAL_FUNC);   -- ERROR: PARM HAS
                                                      -- WRONG TYPE.
          PACKAGE BAD_FORM_PROC_PARM IS
                  NEW GEN_6 (BOOLEAN, ONE_PARM_PROC); -- ERROR: PARM HAS
                                                      -- WRONG TYPE.
     BEGIN    -- B)
          NULL;
     END;      -- B)

-----------------------------------------------------------------------

     DECLARE  -- C) DIFFERENT NUMBER OF PARAMETERS

          PROCEDURE NOT_ENUF_PARS (P : OUT INTEGER) IS
          BEGIN
               NULL;
          END NOT_ENUF_PARS;

          PROCEDURE TOO_MANY_PARS (P : INTEGER;
                                   Q : INTEGER := 0;
                                   R : BOOLEAN := FALSE) IS
          BEGIN
               NULL;
          END TOO_MANY_PARS;

          FUNCTION FUN_TOO_MANY_PARS (P : INTEGER;
                                      Q : BOOLEAN := TRUE)
                                     RETURN BOOLEAN IS
          BEGIN
               RETURN Q;
          END FUN_TOO_MANY_PARS;

          PACKAGE NOT_ENOUGH IS
                  NEW GEN_4 (NOT_ENUF_PARS);          -- ERROR: NOT
                                                      -- ENOUGH PARMS.

          PACKAGE TOO_MANY IS
                  NEW GEN_3 (TOO_MANY_PARS);          -- ERROR: TOO
                                                      -- MANY PARMS.
          PACKAGE TOO_MANY_W_FUN IS
                  NEW GEN_2 (FUN_TOO_MANY_PARS);      -- ERROR: TOO
                                                      -- MANY PARMS.

     BEGIN     -- C)
          NULL;
     END;      -- C)

---------------------------------------------------------------------

     DECLARE   -- D)  PARAMETER MODES

          PROCEDURE IN_VS_OUT (P : IN     INTEGER;
                               Q : IN     CHARACTER;
                               R : IN OUT BOOLEAN) IS
          BEGIN
               NULL;
          END IN_VS_OUT;

          PROCEDURE IN_VS_INOUT (P: IN     INTEGER;
                                 Q: OUT    CHARACTER;
                                 R: IN     BOOLEAN) IS
          BEGIN
               NULL;
          END IN_VS_INOUT;

          PROCEDURE OUT_VS_IN (P: OUT    INTEGER;
                               Q: OUT    CHARACTER;
                               R: IN OUT BOOLEAN) IS
          BEGIN
               NULL;
          END OUT_VS_IN;

          PROCEDURE OUT_VS_INOUT (P: IN     INTEGER;
                                  Q: OUT    CHARACTER;
                                  R: OUT    BOOLEAN) IS
          BEGIN
               NULL;
          END OUT_VS_INOUT;

          PROCEDURE INOUT_VS_IN (P: IN OUT INTEGER;
                                 Q: OUT   CHARACTER;
                                 R: IN    BOOLEAN) IS
          BEGIN
               NULL;
          END INOUT_VS_IN;

          PROCEDURE INOUT_VS_OUT (P: IN     INTEGER;
                                  Q: IN OUT  CHARACTER;
                                  R: IN OUT  BOOLEAN) IS
          BEGIN
               NULL;
          END INOUT_VS_OUT;

          PACKAGE IN_VS_OUT_TST IS
                  NEW GEN_1 (IN_VS_OUT);                 -- ERROR:
                                                         -- WRONG MODE.

          PACKAGE IN_VS_INOUT_TST IS
                  NEW GEN_1 (IN_VS_INOUT);               -- ERROR:
                                                         -- WRONG MODE.

          PACKAGE OUT_VS_IN_TST IS
                  NEW GEN_1 (OUT_VS_IN);                 -- ERROR:
                                                         -- WRONG MODE.

          PACKAGE OUT_VS_INOUT_TST IS
                  NEW GEN_1 (OUT_VS_INOUT);              -- ERROR:
                                                         -- WRONG MODE.

          PACKAGE INOUT_VS_IN_TST IS
                  NEW GEN_1 (INOUT_VS_IN);               -- ERROR:
                                                         -- WRONG MODE.

          PACKAGE INOUT_VS_OUT_TST IS
                  NEW GEN_1 (INOUT_VS_OUT);              -- ERROR:
                                                         -- WRONG MODE.
     BEGIN    -- D)
          NULL;
     END;     -- D)

END BC3604A;
