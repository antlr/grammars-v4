-- B83003C.ADA

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
--     CHECK THAT AN INNER TASK DECLARATION IN THE DECLARATIVE PART OF
--     AN OUTER TASK'S BODY CANNOT HAVE THE SAME IDENTIFIER AS THAT OF
--     AN ENTRY.

-- HISTORY:
--     VCL  02/04/88  CREATED ORIGINAL TEST.

PROCEDURE B83003C IS

BEGIN
     DECLARE
          TASK TYPE TSK2 IS
               ENTRY E9;
          END TSK2;

          TASK BODY TSK2 IS
               TASK TYPE E9;                        -- ERROR: HOMOGRAPH.

          -- BODY FOR THE ABOVE HOMOGRAPH.

               TASK BODY E9 IS                  -- OPTIONAL ERR MESSAGE:
               BEGIN                            --  BODY OF AN INVALID
                    NULL;                       --  TASK TYPE.
               END E9;

          BEGIN
               NULL;
          END TSK2;
     BEGIN
          NULL;
     END;

END B83003C;
