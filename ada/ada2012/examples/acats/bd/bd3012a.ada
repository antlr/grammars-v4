-- BD3012A.ADA

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
--     CHECK THAT EACH CODE IN THE AGGREGATE OF AN ENUMERATION
--     REPRESENTATION SPECIFICATION IS A STATIC, INTEGER EXPRESSION.

-- HISTORY:
--     JKC 04/12/88  CREATED ORIGINAL TEST.

PROCEDURE BD3012A IS
     X   : CONSTANT := 10.0;
     S   : STRING (1..2);
     Z   : INTEGER := 100;
     N   : INTEGER := 1;
     P   : INTEGER := 8;

     TYPE ENUM1 IS (A,B,C);
     FOR ENUM1 USE (1,2,X);                          -- ERROR: X.

     TYPE ENUM2 IS (D,E);
     FOR ENUM2 USE (S'LENGTH,2);                     -- ERROR: S'LENGTH.

     TYPE ENUM3 IS (F,G,H,I);
     FOR ENUM3 USE (1,2,3,Z);                        -- ERROR: Z.

     TYPE ENUM4 IS (J,K,L,M);
     FOR ENUM4 USE (2**N,3,4,5);                     -- ERROR: 2**N.

     TYPE ENUM5 IS (O,Q,R,T,U,V);
     FOR ENUM5 USE (3,4,5,6,7,INTEGER'POS(P));       -- ERROR:
                                                     -- INTEGER'POS(P).

     TYPE ENUM6 IS (W,Y);
     FOR ENUM6 USE (1.0,2);                          -- ERROR: 1.0.

BEGIN
     NULL;
END BD3012A;
