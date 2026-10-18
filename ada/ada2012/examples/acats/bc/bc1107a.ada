-- BC1107A.ADA

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
--     CHECK THAT THE DEFAULT EXPRESSION FOR A GENERIC PARAMETER
--     CANNOT REFER TO A LATER PARAMETER OF THE SAME GENERIC PART
--     OR TO ITSELF.

-- HISTORY:
--     DAT 07/15/81  CREATED ORIGINAL TEST.
--     SPS 10/21/82
--     SPS 02/10/83
--     BCB 08/02/88  MODIFIED HEADER FORMAT AND ADDED CHECK THAT A
--                   GENERIC FORMAL OBJECT'S INITIALIZATION CANNOT
--                   INVOKE A GENERIC FORMAL FUNCTION DECLARED LATER
--                   IN THE SAME GENERIC FORMAL PART.

PROCEDURE BC1107A IS

     GENERIC
          I, J : INTEGER := I;              -- ERROR: J REFS I.
     PACKAGE P1 IS END P1;

     INT_3 : INTEGER := 3;
     FUNCTION P4 (X : INTEGER) RETURN INTEGER;
     GENERIC
          P1 : INTEGER;                      -- OK.
          INT_3 : INTEGER  := INTEGER'(3);   -- OK.
          P1A, P1B : INTEGER := (P1A'SIZE);  -- ERROR: REFS P1A.
          P1C, P1D : INTEGER := P1D'SIZE;    -- ERROR: P1C REFS P1D.
          P2 : INTEGER := P2;                -- ERROR: SELF-REF.
          TYPE INTEGER IS RANGE <> ;
          P2A : INTEGER := INTEGER(P2A'SIZE);-- ERROR: SELF-REF.
          P2D : INTEGER := INTEGER'FIRST;    -- OK.
          P2E : INTEGER := INTEGER(P2D'SIZE);-- OK.
          P4 : INTEGER := P4(3);             -- ERROR: SELF-REF.
          P5 : INTEGER := P6;                -- ERROR: FORWARD REF.
          P6 : INTEGER := INTEGER'LAST;      -- OK
          TYPE P7 IS ARRAY (INTEGER) OF P7;  -- ERROR: SELF-REF.
          TYPE P8 IS ARRAY (INTEGER) OF INTEGER;
          P13: UNDEF := TRUE;                -- ERROR: REFS TO UNDEF.
          TYPE UNDEF IS RANGE <>;
          Q : P8 := (Q'RANGE => 0);          -- ERROR: SELF-REF.
          TYPE U IS ARRAY (INTEGER RANGE <> )-- OK.
               OF INTEGER;
          P10 : U;                           -- OK.
          P11 : U := P10;                    -- 0K.
     PACKAGE P3 IS END P3;

     GENERIC
          FORM_OBJ : INTEGER := FUNC;        -- ERROR: FORWARD REF.
          WITH FUNCTION FUNC RETURN INTEGER;
     PACKAGE PP IS END PP;

     FUNCTION P4 (X:INTEGER) RETURN INTEGER IS
        BEGIN
            RETURN X;
     END P4;

BEGIN
     NULL;
END BC1107A;
