-- BC3016G.ADA

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
--   CHECK THAT AN INSTANTIATED PACKAGE HAS THE PROPERTIES REQUIRED
--   OF A PACKAGE.

--   CHECK THAT IF THE PARENT TYPE IN A DERIVED TYPE DEFINITION IS
--   A GENERIC FORMAL TYPE, THE OPERATIONS DECLARED FOR THE DERIVED
--   TYPE IN THE TEMPLATE ARE DETERMINED BY THE DECLARATION OF THE
--   FORMAL TYPE.  THE OPERATIONS DECLARED FOR DERIVED TYPE IN THE
--   INSTANCE ARE DETERMINED BY THE ACTUAL TYPE DENOTED BY THE FORMAL
--   PARAMETER.  SEE AI-00398.

-- HISTORY:
--   DAS  8 OCT 90   INITIAL VERSION.
--   DTN 26 MAY 92   REMOVED UNNECESSARY REFERENCES TO PACKAGE REPORT.
--   PWN 19 DEC 94   CORRECTED -- ERROR: INCONSITENCIES



PROCEDURE BC3016G IS

     GENERIC
          TYPE DISCRETE IS (<>);
     PACKAGE GP IS
          TYPE AR IS ARRAY (1..5) OF DISCRETE;
          Y1 : AR := ( 1..5 => DISCRETE'FIRST );
          Y2 : AR := Y1 AND Y1;                  -- ERROR: NO LOGICAL
                                                 -- OPERATORS FOR GP.AR

          Y3 : AR := "ABCDE";                    -- ERROR:
     END GP;

BEGIN
     NULL;
END BC3016G;
