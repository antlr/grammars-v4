-- B54A21A.ADA

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
-- CHECK THAT NON-STATIC CASE CHOICES ARE FORBIDDEN.

-- DAT 1/29/81
--  RM 6/29/82
-- JBG 5/23/83
-- PWN 01/31/95 REMOVED INCONSISTENCIES WITH ADA 9X.

PROCEDURE B54A21A IS

     TYPE T IS RANGE 1 .. 10;
     T1 : T RANGE 1 .. 1 := 1;
     T5 : T RANGE 5 .. 5 := 5;
     T10 : T RANGE 10 .. 10 := 10;
     SUBTYPE ST IS T RANGE 10 .. T10;
     SUBTYPE ST1 IS T RANGE T1 .. 1;
     VST : ST RANGE 10 .. 10 := 10;
     C6 : CONSTANT T := 6;
     B  :  BOOLEAN  RANGE TRUE..TRUE  := TRUE ;


BEGIN

     CASE T'(3) IS
          WHEN VST => NULL;                -- ERROR: NON-STATIC CHOICE.
          WHEN 2 => NULL;                  -- OK.
          WHEN ST1 => NULL;                -- ERROR: NON-STATIC CHOICE.
          WHEN T'(3) | 4 => NULL;          -- OK.
          WHEN T RANGE 5 .. T5 => NULL;    -- ERROR: NON-STATIC CHOICE.
          WHEN C6 => NULL;                 -- OK.
          WHEN ST RANGE 10..T10-1 => NULL; -- ERROR: NON-STATIC CHOICE.
          WHEN C6 + 3 - 2 => NULL;         -- OK.
          WHEN VST - 1 .. 9 => NULL;       -- ERROR: NON-STATIC CHOICE.
          WHEN OTHERS => NULL;             -- OK.
     END CASE;


     CASE  TRUE  IS
          WHEN  B       =>  NULL ;         -- ERROR: NON-STATIC CHOICE.
          WHEN  OTHERS  =>  NULL ;         -- OK.
     END CASE;

END B54A21A;
