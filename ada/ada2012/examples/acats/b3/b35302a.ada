-- B35302A.ADA

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
-- CHECK THAT A TYPE HAVING JUST THE ENUMERATION LITERALS 'TRUE' AND
-- 'FALSE' IS NOT CONSIDERED TO BE A BOOLEAN TYPE.

-- RJW 2/14/86

PROCEDURE B35302A IS

     TYPE BOOL IS (FALSE, TRUE);
     B : BOOL :=FALSE;

BEGIN
     IF BOOL'(TRUE) THEN                     -- ERROR: INVALID TYPE 
                                             --        FOR CONDITION.
          NULL;
     ELSIF BOOL'(FALSE) THEN                 -- ERROR: INVALID TYPE 
                                             --        FOR CONDITION.
          NULL;
     END IF;
          
     WHILE BOOL'(TRUE) LOOP                  -- ERROR: INVALID TYPE 
                                             --        FOR CONDITION.
          EXIT WHEN BOOL'(FALSE);            -- ERROR: INVALID TYPE 
                                             --        FOR CONDITION.
     END LOOP;

     B := BOOL'(TRUE) AND BOOL'(FALSE);      -- ERROR: INVALID TYPES 
                                             --        FOR 'AND'.
     B := BOOL'(TRUE) OR  BOOL'(FALSE);      -- ERROR: INVALID TYPES 
                                             --        FOR 'OR'.
     B := BOOL'(TRUE) XOR BOOL'(FALSE);      -- ERROR: INVALID TYPES 
                                             --        FOR 'XOR'.
     B := NOT BOOL'(TRUE);                   -- ERROR: INVALID TYPES 
                                             --        FOR 'NOT'.
     B := BOOL'(TRUE) AND THEN BOOL'(FALSE); -- ERROR: INVALID TYPES 
                                             --        FOR 'AND THEN'.
     B := BOOL'(TRUE) OR ELSE BOOL'(FALSE);  -- ERROR: INVALID TYPES 
                                             --        FOR 'OR ELSE'.
     
     B := (B = B);                           -- ERROR: 'B' NOT BOOLEAN.
     B := (2 >= 1);                          -- ERROR: 'B' NOT BOOLEAN.
     B := (TRUE IN BOOL);                    -- ERROR: 'B' NOT BOOLEAN.

END B35302A;
