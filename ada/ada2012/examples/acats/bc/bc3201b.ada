-- BC3201B.ADA

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
-- CHECK THAT IN A GENERIC INSTANTIATION, A GENERIC ACTUAL TYPE
-- PARAMETER MUST NOT BE A TASK TYPE (OR A TYPE WITH TASK
-- COMPONENTS) IF THE GENERIC FORMAL IS A NONLIMITED PRIVATE TYPE
-- PARAMETER.

-- ASL 8/24/81
-- SPS 7/8/82
-- SPS 12/10/82

 PROCEDURE BC3201B IS

     TASK TYPE T;

     TYPE TASKREC IS
          RECORD
               COMP : T;
          END RECORD;

     TYPE TASKARR IS ARRAY(1..2) OF T;

     SUBTYPE INT IS INTEGER RANGE 1 .. 2;
     TYPE VREC (D : INT) IS
          RECORD
               CASE D IS
               WHEN 1 =>
                    C1 : TASKREC;
               WHEN 2 =>
                    C2 : BOOLEAN;
               END CASE;
          END RECORD;
     SUBTYPE CVREC IS VREC (D=>2);

     TASK BODY T IS
     BEGIN
          NULL;
     END T;

     GENERIC
          TYPE GFT IS PRIVATE;
     PACKAGE TEMPLATE IS
     END TEMPLATE;

     PACKAGE INST1 IS NEW TEMPLATE(T);       -- ERROR: T.
     PACKAGE INST2 IS NEW TEMPLATE(TASKREC); -- ERROR: TASKREC.
     PACKAGE INST3 IS NEW TEMPLATE(TASKARR); -- ERROR: TASKARR.
     PACKAGE INST4 IS NEW TEMPLATE(CVREC);   -- ERROR: CVREC.
     PACKAGE INST5 IS NEW TEMPLATE(VREC);    -- ERROR: VREC.
BEGIN
     NULL;
END BC3201B;
