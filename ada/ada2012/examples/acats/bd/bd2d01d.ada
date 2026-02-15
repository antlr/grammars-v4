-- BD2D01D.ADA

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
--     CHECK THAT A 'SMALL SPECIFICATION CANNOT BE GIVEN FOR A PRIVATE
--     OR INCOMPLETE TYPE PRIOR TO THE FULL DECLARATION.

-- HISTORY:
--     DHH 08/22/88 CREATED ORIGINAL TEST.

PACKAGE BD2D01D IS

     TYPE INCOM;
     TYPE ACC_INCOM IS ACCESS INCOM;

     TYPE PRIV IS PRIVATE;
     TYPE PRIV1 IS PRIVATE;

     FOR INCOM'SMALL USE 0.125;                         -- ERROR:

     TYPE INCOM IS DELTA 0.25 RANGE -1.0 .. 1.0;

     FOR PRIV'SMALL USE 0.125;                          -- ERROR:

PRIVATE
     FOR PRIV1'SMALL USE 0.125;                         -- ERROR:

     TYPE PRIV IS DELTA 0.25 RANGE -1.0 .. 1.0;
     TYPE PRIV1 IS DELTA 0.25 RANGE -1.0 .. 1.0;

END BD2D01D;
