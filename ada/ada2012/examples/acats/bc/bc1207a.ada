-- BC1207A.ADA

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
-- CHECK THAT AN UNCONSTRAINED FORMAL TYPE WITH DISCRIMINANTS IS NOT 
-- ALLOWED IN A VARIABLE DECLARATION, A COMPONENT DECLARATION (OF AN
-- ARRAY OR RECORD), OR AN ALLOCATOR.

-- R.WILLIAMS 9/25/86

PROCEDURE BC1207A IS
     
     GENERIC
          TYPE FORM (D : INTEGER) IS PRIVATE;
          TYPE FARR IS 
                    ARRAY (INTEGER RANGE <>) OF FORM; -- ERROR:
                                                      -- UNCONSTRAINED.
          TYPE FORM_NAME IS ACCESS FORM;             
     PROCEDURE P;

     PROCEDURE P IS
          A : FORM;                                   -- ERROR:
                                                      -- UNCONSTRAINED.
          TYPE REC IS
               RECORD
                    A : FORM;                         -- ERROR:
                                                      -- UNCONSTRAINED.
               END RECORD;

          TYPE ARR IS ARRAY (1 .. 10) OF FORM;        -- ERROR:
                                                      -- UNCONSTRAINED.
          TYPE FORM_NAME1 IS ACCESS FORM (1);

          B : FORM_NAME1 := NEW FORM;                 -- ERROR:
                                                      -- UNCONSTRAINED.
          C : FORM_NAME := NEW FORM;                  -- ERROR:
                                                      -- UNCONSTRAINED.
     BEGIN
          NULL;
     END P;

BEGIN
     NULL;
END BC1207A;
