-- B52002B.ADA

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
-- CHECK THAT THE LEFT SIDE OF AN ASSIGNMENT STMT MUST BE A VARIABLE.

-- CHECK THAT THE LHS MAY NOT BE AN ENUMERATION LITERIAL, BLOCK
--  IDENTIFIER, LOOP IDENTIFIER, STATEMENT LABEL, PACKAGE NAME,
--  TYPE NAME, SUBTYPE, OR PRAGMA IDENTIFIER.

-- JRK 10/30/80
-- SPS 03/21/83

PROCEDURE B52002B IS

     TYPE E IS (E1, E2);
     SUBTYPE ES IS E RANGE E1..E1;

     PACKAGE P IS
          I : INTEGER;
     END P;

BEGIN

     E1 := E2;            -- ERROR: ENUMERATION LITERAL ON LEFT.

     B52002B := B52002B;  -- ERROR: SUBPROGRAM IDENTIFIER ON LEFT.

  B: BEGIN
          B := B;         -- ERROR: BLOCK IDENTIFIER ON LEFT.
     END B;

  F: FOR I IN 1..10 LOOP
          F := F;         -- ERROR: LOOP IDENTIFIER ON LEFT.
     END LOOP F;

<<L>>
     L := L;              -- ERROR: STATEMENT LABEL ON LEFT.

     P := P;              -- ERROR: PACKAGE IDENTIFIER ON LEFT.

     LIST := LIST;        -- ERROR: PRAGMA IDENTIFER ON LEFT.

     E := E;              -- ERROR: TYPE IDENTIFIER ON LEFT.

     ES := ES;            -- ERROR: SUBTYPE IDENTIFIER ON LEFT.

END B52002B;
