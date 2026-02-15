-- B45116A.ADA

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
-- CHECK THAT MULTIDIMENSIONAL ARRAYS AND ONE-DIMENSIONAL NON-BOOLEAN
-- ARRAYS ARE FORBIDDEN AS OPERANDS.

-- JWC 11/15/85
-- PWB 03/04/86  CORRECTED ERROR IN INITIALIZATION OF
--               ARRPRIBL1 AND ARRPRIBL2.

PROCEDURE B45116A IS

     PACKAGE PKG IS
          TYPE PRIVARR_TYPE IS PRIVATE;
          PAC : CONSTANT PRIVARR_TYPE;
          TYPE PRIBOOL_TYPE IS PRIVATE;
          PBC : CONSTANT PRIBOOL_TYPE;
     PRIVATE
          TYPE PRIVARR_TYPE IS ARRAY (1 .. 10) OF BOOLEAN;
          PAC : CONSTANT PRIVARR_TYPE := PRIVARR_TYPE'(OTHERS => TRUE);
          TYPE PRIBOOL_TYPE IS NEW BOOLEAN;
          PBC : CONSTANT PRIBOOL_TYPE := TRUE;
     END PKG;

     TYPE DERBOOLEAN IS NEW BOOLEAN;
     TYPE MATRIX_TYPE IS ARRAY (1 .. 10, 1 .. 10) OF INTEGER;
     TYPE VECTOR_TYPE IS ARRAY (1 .. 10) OF INTEGER;
     TYPE BOOARR_TYPE IS ARRAY (1 .. 10) OF BOOLEAN;
     TYPE BOOMAT_TYPE IS ARRAY (1 .. 10, 1 .. 10) OF BOOLEAN;
     TYPE DERARR_TYPE IS ARRAY (1 .. 10) OF DERBOOLEAN;
     TYPE ARRPRIBOOL_TYPE IS ARRAY (1 .. 10) OF PKG.PRIBOOL_TYPE;

     MATRIX1, MATRIX2 : MATRIX_TYPE :=
                        MATRIX_TYPE'(OTHERS => (OTHERS => 0));
     VECTOR1, VECTOR2 : VECTOR_TYPE :=
                        VECTOR_TYPE'(OTHERS => 0);
     BOOARR1, BOOARR2 : BOOARR_TYPE :=
                        BOOARR_TYPE'(OTHERS => TRUE);
     BOOMAT1, BOOMAT2 : BOOMAT_TYPE :=
                        BOOMAT_TYPE'(OTHERS => (OTHERS => TRUE));
     DERARR1, DERARR2 : DERARR_TYPE :=
                        DERARR_TYPE'(OTHERS => TRUE);
     PRIVARR1,  PRIVARR2  : PKG.PRIVARR_TYPE := PKG.PAC;
     ARRPRIBL1, ARRPRIBL2 : ARRPRIBOOL_TYPE :=
                            ARRPRIBOOL_TYPE'(OTHERS => PKG.PBC);

     TYPE BOOLEAN IS (NEWTRUE, NEWFALSE);
     TYPE NEWARR_TYPE IS ARRAY (1 .. 10) OF BOOLEAN;
     NEWARR1, NEWARR2 : NEWARR_TYPE :=
                        NEWARR_TYPE'(OTHERS => NEWTRUE);

BEGIN

     MATRIX1 := MATRIX1 AND MATRIX2;       -- ERROR: MULTIDIMENSIONAL
                                           -- ARRAY - AND.

     MATRIX1 := MATRIX1 OR MATRIX2;        -- ERROR: MULTIDIMENSIONAL
                                           -- ARRAY - OR.

     MATRIX1 := MATRIX1 XOR MATRIX2;       -- ERROR: MULTIDIMENSIONAL
                                           -- ARRAY - XOR.

     VECTOR1 := VECTOR1 AND VECTOR2;       -- ERROR: ONE-DIMENSIONAL
                                           -- NON-BOOLEAN ARRAY - AND.

     VECTOR1 := VECTOR1 OR VECTOR2;        -- ERROR: ONE-DIMENSIONAL
                                           -- NON BOOLEAN ARRAY - OR.

     VECTOR1 := VECTOR1 XOR VECTOR2;       -- ERROR: ONE-DIMENSIONAL
                                           -- NON BOOLEAN ARRAY - XOR.

     BOOMAT1 := BOOMAT1 AND BOOMAT2;       -- ERROR: MULTI-DIMENSIONAL
                                           -- BOOLEAN ARRAY - AND.

     BOOMAT1 := BOOMAT1 OR BOOMAT2;        -- ERROR: MULTI-DIMENSIONAL
                                           -- BOOLEAN ARRAY - OR.

     BOOMAT1 := BOOMAT1 XOR BOOMAT2;       -- ERROR: MULTI-DIMENSIONAL
                                           -- BOOLEAN ARRAY - XOR.

     PRIVARR1 := PRIVARR1 AND PRIVARR2;    -- ERROR: PRIVATE TYPE AS
                                           -- BOOLEAN ARRAY - AND.

     PRIVARR1 := PRIVARR1 OR PRIVARR2;     -- ERROR: PRIVATE TYPE AS
                                           -- BOOLEAN ARRAY - OR.

     PRIVARR1 := PRIVARR1 XOR PRIVARR2;    -- ERROR: PRIVATE TYPE AS
                                           -- BOOLEAN ARRAY - XOR.

     ARRPRIBL1 := ARRPRIBL1 AND ARRPRIBL2; -- ERROR: ARRAY OF PRIVATE
                                           -- DERIVED BOOLEAN - AND.

     ARRPRIBL1 := ARRPRIBL1 OR ARRPRIBL2;  -- ERROR: ARRAY OF PRIVATE
                                           -- DERIVED BOOLEAN - OR.

     ARRPRIBL1 := ARRPRIBL1 XOR ARRPRIBL2; -- ERROR: ARRAY OF PRIVATE
                                           -- DERIVED BOOLEAN - XOR.

     NEWARR1 := NEWARR1 AND NEWARR2;       -- ERROR: USER REDEFINED
                                           -- BOOLEAN ARRAY - AND.

     NEWARR1 := NEWARR1 OR NEWARR2;        -- ERROR: USER REDEFINED
                                           -- BOOLEAN ARRAY - OR.

     NEWARR1 := NEWARR1 XOR NEWARR2;       -- ERROR: USER REDEFINED
                                           -- BOOLEAN ARRAY - XOR.

     DERARR1 := DERARR1 AND DERARR2;       -- OK.

     DERARR1 := DERARR1 OR DERARR2;        -- OK.

     DERARR1 := DERARR1 XOR DERARR2;       -- OK.

     BOOARR1 := BOOARR1 AND BOOARR2;       -- OK.

     BOOARR1 := BOOARR1 OR BOOARR2;        -- OK.

     BOOARR1 := BOOARR1 XOR BOOARR2;       -- OK.

END B45116A;
