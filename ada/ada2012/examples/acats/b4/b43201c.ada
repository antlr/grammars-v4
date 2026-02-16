-- B43201C.ADA

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
--      CHECK THAT:
--        A) FOR A NON-NULL ARRAY, NO INDEX VALUE BETWEEN THE LOWER AND
--           UPPER BOUND OF THE AGGREGATE CAN BE LEFT UNCOVERED BY THE
--           SET OF CHOICE VALUES.
--        B) AN INDEX VALUE MUST NOT BE REPRESENTED MORE THAN ONCE IN
--           THE SET OF CHOICES.

-- CHANGE HISTORY:
--      16 Feb 1984   JRK
--      28 Dec 1983   EG    Created original test.
--      04 Aug 1988   JET   Revised declaration of Fun2.
--      22 Apr 2021   RLB   Added error location indicators.
--!

PROCEDURE B43201C IS

     TYPE TC IS (RED, GREEN, BLUE);
     TYPE T1 IS ARRAY(1 .. 8) OF TC;
     TYPE T2 IS ARRAY(1 .. 4,1 .. 8) OF TC;
     TYPE T3 IS ARRAY(1 .. 4) OF T1;
     TYPE UT IS ARRAY(INTEGER RANGE <>) OF TC;

     AA1 : T1 := (1 .. 4 => RED,
                  5 | 7 | 8 => BLUE);                  -- ERROR: A. {1:18;1}
     AB1 : T1 := (1 => GREEN, 3 .. 8 => RED);          -- ERROR: A. {18;1}
     AC1, AD1, AE1 : T1;

     AA2 : T2 := (1 .. 4 => (1 | 3 .. 8 => RED));      -- ERROR: A. {18;1}
     AB2 : T2 := ((1 | 2 | 3 => RED, 5 .. 8 => BLUE),  -- ERROR: A. {18;1}
                  (1 .. 8 => GREEN),
                  (1 .. 8 => GREEN),
                  (1 .. 8 => GREEN));
     AC2 : T2 := (1 | 3 .. 4 => (1 .. 8 => RED));      -- ERROR: A. {18;1}
     AD2, AE2, AF2 : T2;

     AA3 : T3 := (1 | 2 | 4 => (1 .. 8 => RED));       -- ERROR: A. {18;1}
     AB3 : T3 := (1 .. 4 => (1 .. 6 | 8 => RED));      -- ERROR: A. {18;1}
     AC3, AD3 : T3;

     BA1 : T1 := (1 .. 5 => RED, 4 => RED,             
                  6 .. 8 => BLUE);                     -- ERROR: B. {1:18;1}
     BB1 : T1 := (1 .. 6 => RED, 4 .. 8 => RED);       -- ERROR: B. {18;1}
     BC1 : T1 := (1 => RED, 2 .. 8 => RED,
                  1 => RED);                           -- ERROR: B. {1:18;1}
     BD1, BE1, BF1 : T1;

     BA2 : T2 := (1 .. 4 => (1 .. 4 => RED,
                             4 .. 5 => RED,            -- ERROR: B. {1:18;1}
                             6 .. 8 => RED));
     BB2 : T2 := (1 .. 4 => (1 .. 8 => RED,
                             4 => RED));               -- ERROR: B. {1:18;1}
     BC2 : T2 := (1 .. 4 => (8 => BLUE, 1 .. 7 => BLUE,
                             8 => BLUE));              -- ERROR: B. {1:18;1}
     BD2 : T2 := (1 .. 4 => (1 .. 8 => RED),
                  3 => (1 .. 8 => RED));               -- ERROR: B. {1:18;1}
     BE2, BF2, BG2, BH2 : T2;

     BA3 : T3 := (3 .. 4 => (1 .. 8 => RED),
                  1 .. 3 => (1 .. 8 => RED));          -- ERROR: B. {1:18;1}
     BB3 : T3 := (1 .. 3 => (6 | 1 .. 8 => RED),       
                  4 => (1 .. 8 => RED));               -- ERROR: B. {1:18;1}
     BC3, BD3 : T3;

     ACA1 : CONSTANT T1 := (5 .. 8 => RED, 1 => RED,
                            2 | 4 => RED);             -- ERROR: A. {1:28;1}
     ACB1 : CONSTANT T1 := (8 => RED, 1 .. 5 => BLUE); -- ERROR: A. {28;1}
     ACC1 : CONSTANT T1 := (5 | 6 | 8 => RED,
                            1 .. 4 => RED);            -- ERROR: A. {1:28;1}

     ACA2 : CONSTANT T2 := (2 .. 4 => (1 .. 8 => RED),
                            1 => (1 | 2 | 8 => RED));  -- ERROR: A. {1:28;1}
     ACB2 : CONSTANT T2 := (1 .. 4 =>
                                 (1 | 8 => RED));      -- ERROR: A. {1:28;1}
     ACC2 : CONSTANT T2 := (1 .. 4 => (1 .. 8 => RED));
     ACD2 : CONSTANT T2 := (4 => (1 .. 8 => RED),
                            1 .. 2 => (1 .. 8 => RED));-- ERROR: A. {1:28;1}

     ACA3 : CONSTANT T3 := (3 .. 4 => (1 .. 8 => RED),
                            1 => (1 .. 8 => RED));     -- ERROR: A. {1:28;1}
     ACB3 : CONSTANT T3 := (1 .. 4 => (6 .. 8 => RED,
                                       1 .. 4 => RED));-- ERROR: A. {1:28;1}
     ACC3 : CONSTANT T3 := (1 .. 4 => (1 .. 8 => RED));

     UCAA : CONSTANT UT := (1 .. 3 => RED);
     UCAB : CONSTANT UT := (2 .. 4 => RED);
     UCAC : CONSTANT UT := (1 .. 2 => RED, 4 => RED);  -- ERROR: A. {28;1}

     BCA1 : CONSTANT T1 := (5 .. 8 => RED,
                            2 .. 3 => RED,
                            1 .. 4 => RED);            -- ERROR: B. {2:28;1}
     BCB1 : CONSTANT T1 := (5 .. 8 => RED,
                            4 .. 6 => RED,             -- ERROR: B. {1:28;1}
                            1 .. 3 => RED);
     BCC1 : CONSTANT T1 := (5 .. 8 => RED,
                            1 .. 4 => RED,
                            6 => RED);                 -- ERROR: B. {2:28;1}

     BCA2 : CONSTANT T2 := ((1 .. 8 => RED),
                            (1 .. 8 => RED),
                            (1 => RED,
                             1 .. 8 => RED),           -- ERROR: B. {3:28;1}
                            (1 .. 8 => RED));
     BCB2 : CONSTANT T2 := (1 => (1 .. 5 => RED,
                                  5 .. 8 => RED),      -- ERROR: B. {1:28;1}
                            2 .. 4 => (1 .. 8 => BLUE));
     BCC2 : CONSTANT T2 := (1 .. 4 => (1 .. 8 => RED,
                                       3 | 6 => RED)); -- ERROR: B. {1:28;1}
     BCD2 : CONSTANT T2 := (4 | 3 | 1 => (1 .. 8 => RED),
                            2 | 3 => (1 .. 8 => RED)); -- ERROR: B. {1:28;1}

     BCA3 : CONSTANT T3 := (2 => (1 .. 8 => RED),
                            3 .. 4 => (1 .. 8 => RED),
                            1 | 2 => (1 .. 8 => RED)); -- ERROR: B. {2:28;1}
     BCB3 : CONSTANT T3 := (1 .. 4 => (5 .. 8 |
                                       1 .. 7 => RED));-- ERROR: B. {1:28;1}

     UCBA : CONSTANT UT := (1 .. 4 => RED,
                            4 => RED);                 -- ERROR: B. {1:28;1}
     UCBB : CONSTANT UT := (2 .. 5 => RED,
                            1 .. 3 => RED);            -- ERROR: B. {1:28;1}
     UCBC : CONSTANT UT := (1 => RED,
                            2 | 3 | 1 => RED);         -- ERROR: B. {1:28;1}

     TYPE TB IS ARRAY(1 .. 4) OF BOOLEAN;
     AA4, AB4, BA4, BB4 : TB;

     AA5, BA5 : STRING(1 .. 5) := "ABCDE";
     AB5, BB5 : STRING(1 .. 9);

     PROCEDURE PROC1 (A1 : T1; A3 : T3) IS
     BEGIN
          NULL;
     END PROC1;

     PROCEDURE PROC2 (A1 : UT) IS
     BEGIN
          NULL;
     END PROC2;

     FUNCTION FUN1 (A1 : T1; A2 : T2) RETURN T1 IS
     BEGIN
          RETURN (1 .. 8 => GREEN);
     END FUN1;

     FUNCTION FUN2 (A1 : T1; A2 : T2) RETURN T2 IS
     BEGIN
          RETURN (1 .. 4 => (1 .. 8 => BLUE));
     END FUN2;

     FUNCTION FUN3 (A1 : UT) RETURN T1 IS
     BEGIN
          RETURN (1 .. 8 => RED);
     END FUN3;

     FUNCTION FUN4 (A1 : T1; A2 : T2) RETURN T1 IS
     BEGIN
          IF ( A1(1) = RED ) THEN
             RETURN (1 .. 3 | 8 => RED);               -- ERROR: A. {21;1}
          ELSE
             RETURN (4 .. 8 => RED, 1 .. 5 => RED);    -- ERROR: B. {21;1}
          END IF;
     END FUN4;

     FUNCTION FUN5 (A1 : T1; A2 : T2) RETURN T2 IS
     BEGIN
          IF ( A1(1) = GREEN ) THEN
             RETURN (1 => (4 .. 8 => BLUE,
                           1 | 2 => GREEN),            -- ERROR: A. {1:21;1}
                     2 .. 4 => (1 .. 3 => RED,
                                4 .. 8 => GREEN));
          ELSE
             RETURN (1 => (4 .. 8 => GREEN,
                           1 .. 5 => GREEN),           -- ERROR: B. {1:21;1}
                     2 .. 4 => (1 .. 8 => RED));
          END IF;
     END FUN5;

     FUNCTION FUN6 (A1 : UT) RETURN UT IS
     BEGIN
          IF ( A1(1) = RED ) THEN
             RETURN (1 | 4 | 3 => RED);                -- ERROR: A. {21;1}
          ELSE
             RETURN (1 .. 4 => RED, 4 => RED);         -- ERROR: B. {21;1}
          END IF;
     END FUN6;

BEGIN
     AC1 := (1 => RED, 2 => RED, 3 => RED, 8 => RED,
             7 => RED, 6 => RED, 5 => RED);            -- ERROR: A. {1:13;1}
     AD1 := (4 | 1 | 2 => RED,
             5 | 3 | 8 | 6 => GREEN);                  -- ERROR: A. {1:13;1}
     AE1 := T1'(6 .. 8 => RED, 4 => RED, 1 .. 3 => RED);  -- ERROR: A. {13;1}
     AD2 := (2 .. 4 => (1 .. 8 => GREEN),
             1 => (1 | 4 | 8 => RED));                 -- ERROR: A. {1:13;1}
     AE2 := (2 .. 4 => (1 .. 8 => RED),
             1 => (5 .. 8 | 1 => BLUE));               -- ERROR: A. {1:13;1}
     AF2 := (3 .. 4 => (1 .. 8 => RED),
             1 => (1 .. 8 => RED));                    -- ERROR: A. {1:13;1}
     AC3 := (4 | 1 => (1 .. 8 => RED),
             2 => (1 .. 8 => RED));                    -- ERROR: A. {1:13;1}
     AD3 := (1 .. 4 => (4 | 2 => RED, 6 .. 8 | 1 => RED,
                        5 => RED));                    -- ERROR: A. {1:13;1}
     BD1 := (1 | 2 => RED, 3 .. 8 => RED,
             1 => RED);                                -- ERROR: B. {1:13;1}
     BE1 := (1 .. 7 => RED, 6 .. 8 => RED);            -- ERROR: B. {13;1}
     BF1 := (1 .. 5 => RED, 6 .. 8 => RED,
             7 => RED);                                -- ERROR: B. {1:13;1}
     BE2 := (1 => (1 => RED, 1 .. 8 => RED),           
             2 .. 4 => (1 .. 8 => GREEN));             -- ERROR: B. {1:13;1}
     BF2 := (1 | 2 .. 4 => (2 .. 8 => RED,
                            1 .. 3 => RED));           -- ERROR: B. {1:13;1}
     BG2 := (1 .. 4 => (1 .. 4 => RED, 5 => RED,
                        6 .. 8 => RED, 5 => RED));     -- ERROR: B. {1:13;1}
     BH2 := (2 .. 4 => (1 .. 8 => RED),
             1 | 3 => (1 .. 8 => RED));                -- ERROR: B. {1:13;1}
     BC3 := (2 | 4 => (1 .. 8 => RED),
             1 .. 3 => (1 .. 8 => RED));               -- ERROR: B. {1:13;1}
     BD3 := (1 .. 4 => (4 .. 6 | 1 .. 2 | 3 |
                        5 .. 8 => RED));               -- ERROR: B. {1:13;1}

     PROC1 (A3 => ACC3,
            A1 => (1 .. 2 | 7 .. 8 => GREEN));         -- ERROR: A. {19;1}
     PROC1 (A3 => ACC3,
            A1 => (4 .. 8 => RED, 1 .. 5 => RED));     -- ERROR: B. {19;1}
     PROC1 (A3 => (1 | 2 .. 4 => (1 .. 4 => RED,
                                  6 .. 8 => RED)),     -- ERROR: A. {1:19;1}
            A1 => (1 .. 8 => GREEN));
     PROC1 (A3 => (1 .. 3 | 4 => (1 .. 5 => RED,
                                  4 .. 8 => RED)),     -- ERROR: B. {1:19;1}
            A1 => (1 .. 8 => GREEN));
     PROC1 (A3 => (1 .. 4 | 2 .. 3 => (1 .. 8 => RED)),-- ERROR: B. {19;1}
            A1 => (1 .. 8 => GREEN));

     PROC2 ((1 | 3 | 4 => RED));                       -- ERROR: A. {12;1}
     PROC2 ((1 .. 4 => RED, 2 .. 3 => BLUE));          -- ERROR: B. {12;1}

     AA1 := FUN1(A2 => ACC2,
                 A1 => (1 | 3 | 4 => RED,
                        5 .. 8 => BLUE));              -- ERROR: A. {1:24;1}
     AA1 := FUN1(A2 => ACC2,
                 A1 => (1 | 3 | 4 => RED,
                        5 .. 8 => RED,
                        1 .. 2 => RED));               -- ERROR: B. {2:24;1}
     AA1 := FUN1(A1 => (1 .. 8 => GREEN),
                 A2 => (2 => (1 .. 6 => BLUE, 8 => RED),  -- ERROR: A. {24;1}
                        3 .. 4 | 1 => (1 .. 8 => GREEN)));
     AA1 := FUN1(A1 => (1 .. 8 => GREEN),
                 A2 => (1 .. 4 => (1 .. 8 => RED,
                                   2 .. 7 => RED)));   -- ERROR: B. {1:24;1}
     AA1 := FUN1(A1 => (1 .. 8 => GREEN),
                 A2 => (4 | 1 .. 3 | 2 => (1 .. 8 => RED)));-- ERROR: B. {24;1}

     AA2 := FUN2(A2 => ACC2,
                 A1 => (1 .. 5 => RED, 8 => RED));     -- ERROR: A. {24;1}
     AA2 := FUN2((1 .. 8 => GREEN, 4 => GREEN),        -- ERROR: B. {17;1}
                 ACC2);
     AA2 := FUN2((1 .. 8 => GREEN),
                 ((7 | 1 .. 6 | 8 => BLUE),
                  (3 .. 8 => RED, 1 => RED),           -- ERROR: A.  {19;1}
                  (1 .. 8 => RED),
                  (1 .. 8 => RED)));
     AA2 := FUN2((1 .. 8 => GREEN),
                 (1 | 2 => (1 .. 4 => RED,
                            3 .. 8 => RED),            -- ERROR: B. {1:18;1}
                  3 .. 4 => (1 .. 8 => RED)));
     AA1 := FUN3((1 | 2 | 4 => RED));                  -- ERROR: A. {17;1}
     AA1 := FUN3((1 => RED, 1 .. 4 => RED));           -- ERROR: B. {17;1}

     AA4 := (FALSE, FALSE, TRUE, TRUE);
     BA4 := AA4;
     AB4 := AA4 AND (1 | 2 => TRUE, 4 => FALSE);       -- ERROR: A. {21;1}
     AB4 := AA4  OR (4 => TRUE, 1 .. 2 => FALSE);      -- ERROR: A. {21;1}
     AB4 := (1 | 4 => TRUE) XOR AA4;                   -- ERROR: A. {13;9}
     BB4 := BA4 AND (1 => TRUE, 1 .. 4 => TRUE);       -- ERROR: B. {21;1}
     BB4 := BA4  OR (2 => TRUE, 1 | 3 => FALSE,
                     2 | 4 => TRUE);                   -- ERROR: B. {1:21;1}
     BB4 := (1 .. 3 => TRUE, 2 .. 4 => TRUE) XOR BA4;  -- ERROR: B. {13;9}
     AB5 := AA5 & (4 => 'B', 1 .. 2 => 'A');           -- ERROR: A. {19;1}
     BB5 := (1 .. 3 => 'A', 2 .. 4 => 'A') & BA5;      -- ERROR: B. {13;7}

     IF ( AA4 = (2 => FALSE, 1 => TRUE,
                 4 => FALSE) ) THEN                    -- ERROR: A. {1:9;5}
        NULL;
     END IF;
     IF ( BA4 < (2 .. 4 => TRUE, 1 => FALSE,
                 3 => TRUE) ) THEN                     -- ERROR: B. {1:9;5}
        NULL;
     END IF;
END B43201C;
