-- B83A01A.ADA

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
-- CHECK THAT A STATEMENT LABEL INSIDE A LOOP OR BLOCK BODY CANNOT BE 
--    THE SAME AS A STATEMENT LABEL OUTSIDE THESE CONSTRUCTS WHEN ALL 
--    LABELS ARE INSIDE THE SAME SUBPROGRAM.

-- RM 02/05/80


PROCEDURE  B83A01A  IS

BEGIN

     << LAB_OUTSIDE_INBLOCK >>                NULL ;
     << LAB_OUTSIDE_INLOOP >>                 NULL ;
     << LAB_OUTSIDE_INBLOCKLOOP >>            NULL ;

     BEGIN

          << LAB_OUTSIDE_INBLOCK >>           NULL ;  -- ERROR: DUPLIC.
          << LAB_INBLOCK_INLOOP >>            NULL ;
          << LAB_INBLOCK_INBLOCKLOOP >>       NULL ;

          FOR  I  IN  INTEGER  LOOP
               << LAB_OUTSIDE_INBLOCKLOOP >>  NULL ;  -- ERROR: DUPLIC.
               << LAB_INBLOCK_INBLOCKLOOP >>  NULL ;  -- ERROR: DUPLIC.
               << LAB_INLOOP_INBLOCKLOOP >>   NULL ;
          END LOOP;

     END ;

     FOR  I  IN  INTEGER  LOOP
          << LAB_OUTSIDE_INLOOP >>            NULL ;  -- ERROR: DUPLIC.
          << LAB_INBLOCK_INLOOP >>            NULL ;  -- ERROR: DUPLIC.
          << LAB_INLOOP_INBLOCKLOOP >>        NULL ;  -- ERROR: DUPLIC.
     END LOOP;


END B83A01A ;
