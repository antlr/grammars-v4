-- BD2A25A.ADA

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
--     CHECK THAT A SIZE SPECIFICATION FOR AN ENUMERATION TYPE,
--     WHERE AN ENUMERATION REPRESENTATION CLAUSE HAS AND HAS
--     NOT BEEN GIVEN, MUST BE REJECTED IF THE SIZE IS TOO SMALL
--     TO REPRESENT ALL THE VALUES.

-- HISTORY:
--     JKC 03/29/88  CREATED ORIGINAL TEST.

PROCEDURE BD2A25A IS

     TYPE BASIC_ENUM IS (A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,
                         U,V,W,X,Y,Z);
     FOR BASIC_ENUM'SIZE USE 4;                     -- ERROR: TOO SMALL.

     TYPE ENUM IS (RED, BLUE, YELLOW, GREEN);
     FOR ENUM USE (RED=>1, BLUE=>2, YELLOW=>3, GREEN=>20);
     FOR ENUM'SIZE USE 2;                           -- ERROR: TOO SMALL.

BEGIN
     NULL;
END BD2A25A;
