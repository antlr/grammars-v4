-- BC3403C.ADA

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
-- WHEN A GENERIC FORMAL TYPE IS AN ARRAY TYPE, CHECK THAT CORRESPONDING
-- INDEX POSITIONS HAVE THE SAME BASE TYPE.

-- CHECK WHEN THE INDEX BASE TYPE IS A GENERIC FORMAL PARAMETER DECLARED
-- IN AN ENCLOSING GENERIC UNIT.

-- SPS 6/8/82

PROCEDURE BC3403C IS

     GENERIC
          TYPE INDEX IS (<>);
     PACKAGE PACK IS 
          SUBTYPE IND IS INDEX;
          TYPE NIND IS NEW INDEX;
          SUBTYPE NATURAL IS INTEGER RANGE 1..INTEGER'LAST;
          TYPE AI IS ARRAY (INDEX) OF NATURAL;
          TYPE AIU IS ARRAY (INDEX RANGE <>) OF NATURAL;
          TYPE AIND IS ARRAY (IND) OF NATURAL;
          TYPE ANIND IS ARRAY (NIND) OF NATURAL;
          TYPE AN IS ARRAY (NATURAL) OF NATURAL;
          TYPE A_I IS ARRAY (INTEGER) OF NATURAL;
          TYPE AB IS ARRAY (BOOLEAN) OF NATURAL;
          TYPE AC IS ARRAY (CHARACTER) OF NATURAL;

          GENERIC
               TYPE AR IS ARRAY (INDEX) OF NATURAL;
          PACKAGE P IS END P;

          PACKAGE P1 IS NEW P (AI);          -- OK.
          PACKAGE P2 IS NEW P (A_I);         -- ERROR: INTEGER.
          PACKAGE P3 IS NEW P (AN);          -- ERROR: NATURAL.
          PACKAGE P4 IS NEW P (AB);          -- ERROR: BOOLEAN.
          PACKAGE P5 IS NEW P (AC);          -- ERROR: CHARACTER.
          PACKAGE P6 IS NEW P (AIND);        -- OK.
          PACKAGE P7 IS NEW P (ANIND);       -- ERROR: NIND.

     END PACK;

BEGIN
     NULL;
END BC3403C;
