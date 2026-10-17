-- B74202A.ADA

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
--     FOR ARRAYS OF NON-LIMITED PRIVATE TYPES, OPERATIONS WHICH DEPEND
--     ON CHARACTERISTICS OF THE FULL DECLARATION ARE NOT ACCESSIBLE
--     FROM OUTSIDE THE PACKAGE.

-- HISTORY:
--     BCB 03/10/88  CREATED ORIGINAL TEST.

PROCEDURE B74202A IS

     PACKAGE P IS
          TYPE INT IS PRIVATE;
          TYPE B IS PRIVATE;
          TYPE S IS PRIVATE;
          TYPE STR IS ARRAY(1..10) OF S;
          TYPE BOOL IS ARRAY(1..5) OF B;
          TYPE ARR IS ARRAY(1..5) OF INT;
     PRIVATE
          TYPE INT IS RANGE 1 .. 100;
          TYPE S IS NEW CHARACTER;
          TYPE B IS NEW BOOLEAN;
     END P;

     USE P;

     AR, BR : ARR;

     CR, DR : BOOL;

     ST : STR;

BEGIN
     IF AR < BR THEN                       -- ERROR: < OPERATOR.
          NULL;
     END IF;

     IF AR <= BR THEN                      -- ERROR: <= OPERATOR.
          NULL;
     END IF;

     IF AR > BR THEN                       -- ERROR: > OPERATOR.
          NULL;
     END IF;

     IF AR >= BR THEN                      -- ERROR: >= OPERATOR.
          NULL;
     END IF;

     IF CR AND DR THEN                     -- ERROR: AND OPERATOR.
          NULL;
     END IF;

     IF CR OR DR THEN                      -- ERROR: OR OPERATOR.
          NULL;
     END IF;

     IF CR XOR DR THEN                     -- ERROR: XOR OPERATOR.
          NULL;
     END IF;

     ST := "MY MESSAGE";                   -- ERROR: STRING LITERAL.

END B74202A;
