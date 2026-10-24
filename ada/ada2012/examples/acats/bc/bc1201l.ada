-- BC1201L.ADA

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
-- CHECK THAT A RANGE OF FORM T'RANGE IS NOT ALLOWED 
-- AS AN INDEX RANGE IN A GENERIC FORMAL ARRAY TYPE DECLARATION.

-- PWB  2/11/86

PROCEDURE BC1201L IS

     TYPE SMALL IS ARRAY (1..10) OF BOOLEAN;
     TYPE ENUM  IS (ONE, TWO, THREE);
     TYPE ENUM_ARRAY IS 
          ARRAY (ENUM RANGE <>) OF INTEGER;

     GENERIC
          TYPE BASE IS PRIVATE;
          LIST : ENUM_ARRAY;                      -- O.K.
          TYPE ARRAY_1 IS
               ARRAY (LIST'RANGE) OF BASE;        -- ERROR: 'RANGE.
     PROCEDURE GEN_PROC (X : INTEGER);

     GENERIC
          TYPE BASE IS (<>);
          TYPE ARRAY_1 IS
               ARRAY (BASE) OF INTEGER;           -- O.K.
          TYPE ARRAY_2 IS
               ARRAY (ARRAY_1'RANGE) OF BOOLEAN;  -- ERROR: 'RANGE.
     FUNCTION GEN_FUNC (X : INTEGER) 
          RETURN BOOLEAN;

     GENERIC
          TYPE BASE IS RANGE <>;
          TYPE ARRAY_1 IS 
               ARRAY (SMALL'RANGE) OF BASE;       -- ERROR: 'RANGE.
     PACKAGE GEN_PACK IS
     END GEN_PACK;

     PROCEDURE GEN_PROC (X : INTEGER) IS
     BEGIN
          NULL;
     END GEN_PROC;

     FUNCTION GEN_FUNC (X : INTEGER) RETURN BOOLEAN IS
     BEGIN
          RETURN (X=5);
     END GEN_FUNC;

BEGIN    -- BC1201L
     NULL;
END BC1201L;
