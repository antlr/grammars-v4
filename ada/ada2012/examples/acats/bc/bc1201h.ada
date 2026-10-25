-- BC1201H.ADA

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
-- CHECK THAT AN INDEX CONSTRAINT IS NOT ALLOWED ON A COMPONENT
-- TYPE SPECIFICATION IN A GENERIC FORMAL ARRAY TYPE DECLARATION.

-- CHANGE HISTORY:
--      10 Feb 1986   PWB
--      22 Apr 2021   RLB   Added error location indicators.
--!

PROCEDURE BC1201H IS

     TYPE B_I IS 
          ARRAY (INTEGER RANGE <>) OF BOOLEAN;
     TYPE I_B IS 
          ARRAY (BOOLEAN RANGE <>) OF INTEGER;
     TYPE ENUM IS (ONE, TWO, THREE);
     TYPE I_B_ENUM IS 
          ARRAY (BOOLEAN RANGE <>, ENUM RANGE <>) 
          OF INTEGER;

     GENERIC
          TYPE B_I_ARRAY IS 
               ARRAY (ENUM RANGE <>) OF 
                    B_I (1..5);                 -- ERROR: CONSTRAINT. {2:11;1}
          TYPE STRING_ARRAY IS
               ARRAY (BOOLEAN RANGE <>) OF 
                    STRING(1..20);              -- ERROR: CONSTRAINT. {2:11;1}
     PROCEDURE GEN_PROC (X : INTEGER);

     GENERIC
          TYPE I_B_ARRAY IS
               ARRAY (ENUM RANGE <>) OF
                    I_B (BOOLEAN);              -- ERROR: CONSTRAINT. {2:11;1}
          TYPE I_B_E_ARRAY IS
               ARRAY (INTEGER RANGE <>) OF 
                    I_B_ENUM (TRUE..TRUE, 
                         ENUM'FIRST..ENUM'LAST);-- ERROR: CONSTRAINT. {3:11;1}
     FUNCTION GEN_FUNC (X : INTEGER) 
                       RETURN BOOLEAN;

     GENERIC
          TYPE NULL_ENTRIES IS 
               ARRAY (BOOLEAN RANGE <>) OF 
                    I_B (TRUE..FALSE);          -- ERROR: CONSTRAINT. {2:11;1}
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

BEGIN    -- BC1201H
     NULL;
END BC1201H;
