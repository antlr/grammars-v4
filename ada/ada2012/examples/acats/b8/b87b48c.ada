-- B87B48C.ADA

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
-- CHECK THAT OVERLOADING RESOLUTION USES THE RULE THAT:
--
-- MODES OF PARAMETERS CANNOT RESOLVE OVERLOADING OF SUBPROGRAMS.
  
-- TRH  19 AUG 82
-- JRL  06/07/96  Changed initialization of object F1 to value guaranteed to
--                be in the base range of the type FIXED. Did the same for
--                actual parameter in call to P involving qualified expression
--                of type FIXED.
   
PROCEDURE B87B48C IS
  
     TYPE FIXED IS DELTA 0.1 RANGE -1.0 .. 1.0;
     TYPE LINK  IS ACCESS FIXED;
     TYPE COLOR IS (RED, YELLOW, BLUE);
     TYPE AGE IS NEW INTEGER RANGE 0 .. 120;
 
     A1 : CONSTANT AGE := 18;
     F1 : CONSTANT FIXED := 0.0;
 
     PROCEDURE P (X : OUT STRING) IS
     BEGIN
          NULL;
     END P;
 
     PROCEDURE P (X : IN STRING; Y : FLOAT := 0.0) IS
     BEGIN
          NULL;
     END P;
 
     PROCEDURE P (X : IN LINK) IS
     BEGIN
          NULL;
     END P;
 
     PROCEDURE P (X : OUT LINK; Y : BOOLEAN := FALSE) IS
     BEGIN
          NULL;
     END P;

     PROCEDURE P (X : OUT CHARACTER) IS
     BEGIN
          NULL;
     END P;
 
     PROCEDURE P (X : IN CHARACTER; Y : FIXED := 0.0) IS
     BEGIN
          NULL;
     END P;
 
     PROCEDURE P (X : IN BOOLEAN) IS
     BEGIN
          NULL;
     END P;
 
     PROCEDURE P (X : IN OUT BOOLEAN; Y : COLOR := RED) IS
     BEGIN
          NULL;
     END P;
 
     PROCEDURE P (X : IN OUT AGE) IS
     BEGIN
          NULL;
     END P;
 
     PROCEDURE P (X : IN AGE; Y : STRING := "STRING") IS
     BEGIN
          NULL;
     END P;
 
     PROCEDURE P (X : OUT FIXED) IS
     BEGIN
          NULL;
     END P;
 
     PROCEDURE P (X : IN FIXED; Y : INTEGER := 1) IS
     BEGIN
          NULL;
     END P;
 
     PROCEDURE P (X : IN OUT FLOAT) IS
     BEGIN
          NULL;
     END P;
 
     PROCEDURE P (X : IN FLOAT; Y : AGE := 11) IS
     BEGIN
          NULL;
     END P;
 
     PROCEDURE P (X : IN COLOR) IS
     BEGIN
          NULL;
     END P;
 
     PROCEDURE P (X : OUT COLOR; Y : LINK := NULL) IS
     BEGIN
          NULL;
     END P;
 
BEGIN
   
     P ("STRING");        -- ERROR: AMBIGUOUS PROCEDURE CALL.
 
     P (NULL);            -- ERROR: AMBIGUOUS PROCEDURE CALL.
 
     P ('+');             -- ERROR: AMBIGUOUS PROCEDURE CALL.

     P (TRUE);            -- ERROR: AMBIGUOUS PROCEDURE CALL.
 
     P (21);              -- ERROR: AMBIGUOUS PROCEDURE CALL.
 
     P (FIXED'(0.0));     -- ERROR: AMBIGUOUS PROCEDURE CALL.
   
     P (FLOAT'(1.0));     -- ERROR: AMBIGUOUS PROCEDURE CALL.
 
     P (YELLOW);          -- ERROR: AMBIGUOUS PROCEDURE CALL.
  
     P (A1);              -- ERROR: AMBIGUOUS PROCEDURE CALL.
 
     P (F1);              -- ERROR: AMBIGUOUS PROCEDURE CALL.
 
END B87B48C;
