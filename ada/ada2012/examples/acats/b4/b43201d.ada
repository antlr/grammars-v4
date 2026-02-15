-- B43201D.ADA

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
-- CHECK THAT:

--   A) THE TYPE OF A CHOICE MUST BE THE SAME AS THE CORRESPONDING
--      INDEX TYPE.

--   B) THE TYPE OF THE EXPRESSION SPECIFYING AN ARRAY COMPONENT
--      VALUE MUST BE THE SAME AS THE TYPE OF THE ARRAY COMPONENT.

-- EG  12/29/83
-- JBG 3/30/84
-- JRK 6/24/86   REVISED TO NOT USE OBJECTS WITH ILLEGAL DECLARATIONS
--               IN SUBSEQUENT ERROR TESTS.

PROCEDURE B43201D IS

     TYPE TC IS (RED, GREEN, BLUE);
     TYPE T1 IS ARRAY(1 .. 3) OF TC;
     TYPE T2 IS ARRAY(1 .. 2, 1 .. 3) OF TC;

     AA1 : T1 := (RED => BLUE, 2 .. 3 => RED);         -- ERROR: A.
     AB1 : T1 := (1 .. 2 => RED, BLUE => RED);         -- ERROR: A.
     AC1 : T1 := (1 | 3  => RED, RED => BLUE);         -- ERROR: A.
     AD1, AE1 : T1;

     AA2 : T2 := (1 .. 2 => (RED .. BLUE => GREEN));   -- ERROR: A.
     AB2, AC2 : T2;

     BA1 : T1 := (BLUE, RED, 12);                      -- ERROR: B.
     BB1 : T1 := (10, RED, BLUE);                      -- ERROR: B.
     BC1 : T1 := (RED, 5, GREEN);                      -- ERROR: B.
     BD1 : T1 := (RED, BLUE, RED, OTHERS => 10);       -- ERROR: B.
     BE1 : T1 := (RED, BLUE, RED, OTHERS => GREEN);    -- OK.

     BA2 : T2 := (1 .. 2 => (1 .. 2 => RED, 3 => 5));  -- ERROR: B.
     BB2 : T2 := ((1 .. 3 => RED), (1 .. 3 => 15));    -- ERROR: B.
     BC2 : T2 := (1 .. 2 => (1 .. 3 => 2));            -- ERROR: B.
     BD2 : T2 := (1 .. 2 => (RED, BLUE, 2));           -- ERROR: B.
     BE2 : T2;

     ACA1 : CONSTANT T1 := (GREEN => BLUE,             -- ERROR: A.
                            1 | 2 => RED);
     ACB1 : CONSTANT T1 := (1 .. 3 => RED);

     ACA2 : CONSTANT T2 := (GREEN => (1 .. 3 => BLUE), -- ERROR: A.
                            2 => (1 .. 3 => RED));
     ACB2 : CONSTANT T2 := (1 .. 2 => (1 .. 3 => RED));

     BCA1 : CONSTANT T1 := (12, 11, 10);               -- ERROR: B.
     BCB1 : CONSTANT T1 := (1 .. 3 => RED);

     BCA2 : CONSTANT T2 := (1 => (RED, GREEN, BLUE),
                            2 => (14, RED, 15));       -- ERROR: B.
     BCB2 : CONSTANT T2 := (1 .. 2 => (1 .. 3 => RED));

     TYPE TB IS ARRAY(1 .. 4) OF BOOLEAN;
     AA3, AB3, BA3, BB3 : TB;

     AA4, BA4 : STRING(1 .. 5) := "ABCDE";
     AB4, BB4 : STRING(1 .. 9);

     PROCEDURE PROC1 (A1 : T1; A2 : T2) IS
     BEGIN
          NULL;
     END PROC1;

     FUNCTION FUN1 (A1 : T1; A2 : T2) RETURN T1 IS
     BEGIN
          RETURN (1 .. 3 => GREEN);
     END FUN1;

     FUNCTION FUN2 (A1 : T1; A2 : T2) RETURN T2 IS
     BEGIN
          RETURN (1 .. 2 => (1 .. 3 => BLUE));
     END FUN2;

     FUNCTION FUN3 (A1 : T1; A2 : T2) RETURN T1 IS
     BEGIN
          IF ( A1(1) = RED ) THEN
             RETURN (RED => BLUE, 2 .. 3 => GREEN);    -- ERROR: A.
          ELSE
             RETURN (2 .. 3 => RED, 1 => 16);          -- ERROR: B.
          END IF;
     END FUN3;

     FUNCTION FUN4 (A1 : T1; A2 : T2) RETURN T2 IS
     BEGIN
          IF ( A1(1) = GREEN ) THEN
             RETURN (1 => (RED => RED,                 -- ERROR: A.
                           1 | 3 => BLUE),
                     2 => (1 .. 3 => RED));
          ELSE
             RETURN (1 => (1 .. 3 => GREEN),
                     2 => (1 .. 3 => RED),
                     OTHERS => (1 .. 3 => 6));         -- ERROR: B.
          END IF;
     END FUN4;

BEGIN
     AD1 := (RED => GREEN, GREEN => RED, BLUE => RED); -- ERROR: A.
     AE1 := (RED | 2 => BLUE, 1 => RED);               -- ERROR: A.
     AB2 := (1 .. 2 => (RED .. BLUE => GREEN));        -- ERROR: A.
     AC2 := ((1 | 3 => RED, GREEN => BLUE),            -- ERROR: A.
             (1 | 3 | 2 => GREEN));
     BE1 := (BLUE, RED, GREEN, OTHERS => 22);          -- ERROR: B.
     BE2 := T2'(1 .. 2 => (1 .. 3 => RED, OTHERS => 24)); -- ERROR: B.

     PROC1 (A2 => ACB2,
            A1 => (RED .. GREEN => GREEN));            -- ERROR: A.
     PROC1 (A2 => ACB2,
            A1 => (1 .. 3 => RED, OTHERS => 12));      -- ERROR: B.
     PROC1 (A2 => (1 | 2 => (1 .. 2 => RED, RED => BLUE)), -- ERROR: A.
            A1 => (1 .. 3 => GREEN));
     PROC1 (A2 => (1 | 2 => (33, 34, 35)),             -- ERROR: B.
            A1 => (1 .. 3 => GREEN));

     AD1 := FUN1(A2 => ACB2,
                 A1 => (1 | 3 => RED,
                        BLUE => BLUE));                -- ERROR: A.
     AE1 := FUN1(A2 => ACB2,
                 A1 => (1 | 3 | 2 => 12));             -- ERROR: B.

     AB2 := FUN2(ACB1, ((RED .. BLUE => BLUE),         -- ERROR: A.
                        (1 .. 3 => RED)));
     AC2 := FUN2(ACB1, (1 | 2 => (1 .. 3 => RED),
                        OTHERS => (1 .. 3 => 33)));    -- ERROR: B.

     AA3 := (FALSE, FALSE, TRUE, TRUE);
     BA3 := AA3;
     AB3 := AA3 AND (2 .. 4 => TRUE, RED => TRUE);     -- ERROR: A.
     AB3 := AA3  OR (RED => TRUE, 1 .. 3 => FALSE);    -- ERROR: A.
     AB3 := (RED | GREEN => FALSE, 3 .. 4 => TRUE)     -- ERROR: A.
            XOR AA3;
     BB3 := BA3 AND (12, RED, FALSE, TRUE);            -- ERROR: B.
     BB3 := BA3  OR (4 => RED, 1 .. 3 => FALSE);       -- ERROR: B.
     BB3 := (1 => GREEN, 2 .. 4 => TRUE) XOR BA3;      -- ERROR: B.
     AB4 := AA4 & (RED => 'B', 2 .. 4 => 'A');         -- ERROR: A.
     BB4 := (1 => RED, 2 .. 4 => 'A') & BA4;           -- ERROR: B.

     IF ( AA3 = (2 => FALSE, 1 => TRUE, BLUE => TRUE,  -- ERROR: A.
                 4 => FALSE) ) THEN
        NULL;
     END IF;
     IF ( BA3 < (1 | 3 | 4 => TRUE, 2 => 2) ) THEN     -- ERROR: B.
        NULL;
     END IF;
END B43201D;
