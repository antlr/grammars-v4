-- B64002A.ADA

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
-- FOR FUNCTIONS AND PROCEDURES HAVING NO DEFAULT PARAMETER VALUES,
--   CHECK THAT THE NUMBER OF ACTUAL PARAMETERS AND NAMED PARAMETERS 
--   MUST EQUAL THE NUMBER OF FORMAL PARAMETERS.

-- DAS  1/27/81
-- CPP  6/28/84

PROCEDURE B64002A IS

     A,B,C     : INTEGER := 0;

     PROCEDURE P1 (X : IN INTEGER) IS
     BEGIN
          NULL;
     END P1;

     FUNCTION F2 (X,Y : INTEGER) RETURN INTEGER IS
     BEGIN
          RETURN 2;
     END F2;

     PROCEDURE P3 (X: IN INTEGER; Y: IN OUT INTEGER; Z: OUT INTEGER) IS
     BEGIN
          NULL;
     END P3;

BEGIN

     P1;                           -- ERROR: TOO FEW ARGS.
     NULL;
     P1 (1,2);                     -- ERROR: TOO MANY ARGS.
     NULL;
     A := F2(1);                   -- ERROR: TOO FEW ARGS.
     NULL;
     A := F2(1,2,3);               -- ERROR: TOO MANY ARGS.
     NULL;
     A := F2(X=>1);                -- ERROR: TOO FEW ARGS.
     NULL;
     P3 (1,A);                     -- ERROR: TOO FEW ARGS.
     NULL;
     P3 (X=>1,Y=>A);               -- ERROR: TOO FEW ARGS.
     NULL;
     P3 (1,Y=>A);                  -- ERROR: TOO FEW ARGS.

END B64002A;
