-- B83026B.ADA

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
--     IF A DECLARATION IN THE DECLARATIVE REGION OF AN ENTRY HIDES
--     AN OUTER DECLARATION OF A HOMOGRAPH, THEN CHECK THAT A USE OF THE
--     COMMON IDENTIFIER WHICH WOULD BE A LEGAL REFERENCE TO THE OUTER
--     DECLARATION MUST BE REJECTED IF IT IS ILLEGAL AS A REFERENCE TO
--     THE INNER.

-- HISTORY:
--     BCB 09/01/88  CREATED ORIGINAL TEST.
--     RJW 10/26/89  CHANGED ONE "-- ERROR:" COMMENT TO "-- OPTIONAL
--                   ERR MESSAGE."
--     JRL 05/28/92  ELIMINATED "OPTIONAL ERR MESSAGE" FROM BLOCK ONE
--                   IN LIGHT OF AI-00865. ELIMINATED REDUNDANT
--                   TESTING.

PROCEDURE B83026B IS

     GENERIC
          TYPE T IS PRIVATE;
          X : T;
     FUNCTION GEN_FUN RETURN T;

     FUNCTION GEN_FUN RETURN T IS
     BEGIN
          RETURN X;
     END GEN_FUN;

BEGIN

     ONE:
     DECLARE
          INT : INTEGER := 1;
          FLO : FLOAT := 6.25;

          FUNCTION F IS NEW GEN_FUN (INTEGER, INT);

          TASK INNER IS
               ENTRY HERE (X : INTEGER := F;
                           F : FLOAT);
          END INNER;

          FUNCTION F IS NEW GEN_FUN (FLOAT, FLO);

          TASK BODY INNER IS
          BEGIN
               ACCEPT HERE (X : INTEGER := F;   -- OK.
                            F : FLOAT);
          END INNER;

     BEGIN  -- ONE
          NULL;
     END ONE;

     TWO:
     DECLARE
          INT : INTEGER := 1;
          FLO : FLOAT := 6.25;

          FUNCTION F IS NEW GEN_FUN (INTEGER, INT);

          TASK INNER IS
               ENTRY HERE (F : FLOAT;
                           X : INTEGER := F);          -- ERROR:
          END INNER;

          FUNCTION F IS NEW GEN_FUN (FLOAT, FLO);

          TASK BODY INNER IS
          BEGIN
               ACCEPT HERE (F : FLOAT;
                            X : INTEGER := F);-- OPTIONAL ERR MESSAGE.
          END INNER;

     BEGIN  -- TWO
          NULL;
     END TWO;

END B83026B;
