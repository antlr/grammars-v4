-- B24204B.ADA

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
--     CHECK THAT THE DIGITS AND EXTENDED_DIGITS OF A FLOATING POINT
--     LITERAL ARE WITHIN THE CORRECT RANGE FOR THE NUMBER'S BASE.
--     THIS TEST CHECKS FOR BASE 10 LITERALS.

-- HISTORY:
--     JRK 04/21/80
--     JRK 10/27/80
--     JWC 06/28/85  RENAMED FROM B24104B.ADA.
--     DWC 09/22/87  MOVED CHECKS FOR LITERALS WITH BASES OTHER
--                   THAN 10 TO B24204E.ADA.

PROCEDURE B24204B IS

        F : FLOAT;

BEGIN

        F := 3A.5;              -- ERROR: 3A.5
        F := 3.0A1;             -- ERROR: 3.0A1
        F := 0.3E1A;            -- ERROR: 0.3E1A
        F := 0A#2.3#;           -- ERROR: 0A#2.3#

END B24204B;
