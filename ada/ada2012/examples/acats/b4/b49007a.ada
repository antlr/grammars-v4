-- B49007A.ADA

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
-- CHECK THAT A STATIC EXPRESSION MUST NOT CONTAIN A NAME DENOTING 
-- CERTAIN CONSTANTS, IN PARTICULAR: A) A LOOP PARAMETER,
-- B) A SUBPROGRAM IN PARAMETER, C) AN ENTRY IN PARAMETER,
-- D) A GENERIC FORMAL IN PARAMETER, E) A DEFERRED CONSTANT,
-- F) A CONSTANT OF A PRIVATE TYPE.

-- L.BROWN  08/22/86

PROCEDURE  B49007A  IS

     TYPE REAL IS DIGITS 4;
     TYPE INT IS RANGE 1 .. 10;
     OBJ_INT : INT;
BEGIN

     DECLARE
          TASK TSK IS
               ENTRY ENT(PARM1 : IN INTEGER; PARM2 : IN REAL);
          END;

          TASK BODY TSK IS
               CAS_OBJ : INTEGER ;
          BEGIN
               ACCEPT ENT(PARM1 : IN INTEGER;
                          PARM2 : IN REAL )   DO
                    DECLARE
                         TYPE ITN IS RANGE 1 .. PARM1;    -- ERROR: (C).
                         TYPE FLT IS DIGITS PARM1;        -- ERROR: (C).
                         TYPE FIX IS DELTA PARM2          -- ERROR: (C).
                              RANGE 0.0 .. 5.0;
                         TYPE FIX1 IS DELTA 3.0 
                              RANGE 0.0 .. PARM2;         -- ERROR: (C).
                    BEGIN
                         CASE CAS_OBJ IS
                              WHEN PARM1 =>               -- ERROR: (C).
                                   CAS_OBJ := 9;
                              WHEN OTHERS =>
                                   NULL;
                         END CASE;
                    END ;
               END ENT;
          END TSK;
     BEGIN
          TSK.ENT(2,3.0);
     END;

     DECLARE
          GENERIC
               OBJ1 : IN INT;
               OBJ2 : IN REAL;
          PACKAGE PACK IS
               TYPE TYP IS PRIVATE;
               TYP_CON : CONSTANT TYP;
          PRIVATE
               TYPE TYP IS RANGE 1 .. 10;
               TYPE IDT IS RANGE 1 .. TYP_CON;            -- ERROR: (E).
               TYP_CON : CONSTANT TYP := 5;
               TYPE ER_REC(AT1 : INT := 4) IS RECORD
                    ATN : INTEGER RANGE 1 .. 10 := 3;
                    CASE AT1 IS
                         WHEN OBJ1 =>                     -- ERROR: (D).
                              TR : INTEGER RANGE 1 .. 10;
                         WHEN OTHERS =>
                              TY : BOOLEAN := TRUE;
                    END CASE;
               END RECORD;
               TYPE FLT IS DELTA OBJ2 RANGE 0.0 .. 5.0;   -- ERROR: (D).
               TYPE ITN IS RANGE 1 .. OBJ1;               -- ERROR: (D).
          END PACK;
     BEGIN
          DECLARE
               PACKAGE PACK1 IS NEW PACK(2,3.4);
               USE PACK1;
               X : TYP := TYP_CON;
               CAS_OBJX : BOOLEAN := TRUE;
          BEGIN
               CASE CAS_OBJX IS
                    WHEN (TYP_CON = X) =>                 -- ERROR: (F).
                         CAS_OBJX := FALSE;
                    WHEN OTHERS =>
                         NULL;
               END CASE;
          END;
     END;

     FOR J IN 0 .. 10 LOOP
          DECLARE
               TYPE NGH IS RANGE 1 .. J;                  -- ERROR: (A).
               CAS_OBJ : INT;
          BEGIN
               NULL;
          END;
     END LOOP;

     DECLARE
          PROCEDURE PROC(ITM : IN INT; FIT : IN REAL) IS
               TYPE HJK IS RANGE 1 .. ITM;                -- ERROR: (B).
               TYPE FLYT IS DELTA FIT RANGE 0.0 .. 2.4;   -- ERROR: (B).
               OBJ_CAS : INT;
               TYPE FIXED IS DIGITS ITM;                  -- ERROR: (B).
          BEGIN
               CASE OBJ_CAS IS
                    WHEN ITM =>                      -- ERROR: (B).
                         OBJ_CAS := 2;
                    WHEN OTHERS =>
                         NULL;
               END CASE;
          END PROC;
     BEGIN
          NULL;
     END;

END B49007A;
