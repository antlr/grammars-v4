-- BA1020C0M.ADA

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
-- CHECK THAT IF A GENERIC OR NON-GENERIC PACKAGE P HAS BEEN COMPILED,
-- SUBSEQUENT COMPILATION OF A SUBPROGRAM BODY OR DECLARATION FOR P
-- REMOVES THE PACKAGE DECLARATION FROM THE LIBRARY.

-- SEPARATE FILES ARE:
--     BA1020C0M  THE MAIN PROCEDURE, WITH PACKAGE DECLARATIONS
--                BA1020C1 AND BA1020C2.
--     BA1020C1   A LIBRARY PROCEDURE BODY.
--     BA1020C2   A LIBRARY PROCEDURE DECLARATION.
--     BA1020C3   A LIBRARY PACKAGE BODY (BA1020C1).
--     BA1020C4   A LIBRARY PACKAGE BODY (BA1020C2).
--     BA1020C5   A LIBRARY PROCEDURE BODY (BA1020C2).

-- BHS 7/26/84

PROCEDURE BA1020C0M IS
BEGIN

     NULL;

END BA1020C0M;


PACKAGE BA1020C1 IS

     PROCEDURE P1 (X : INTEGER);

END BA1020C1;


GENERIC
     TYPE ITEM IS RANGE <>;
     I : ITEM;
PACKAGE BA1020C2 IS

     PROCEDURE P2 (Y : ITEM);

END BA1020C2;

PACKAGE BODY BA1020C2 IS
     
     PROCEDURE P2 (Y : ITEM) IS
     BEGIN
          NULL;
     END P2;

END BA1020C2;
