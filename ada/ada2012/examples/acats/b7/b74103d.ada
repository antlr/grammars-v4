-- B74103D.ADA

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
-- CHECK THAT BEFORE THE FULL DECLARATION OF A PRIVATE TYPE, 

     -- (1) THE NAME OF THE PRIVATE TYPE,
     -- (2) A NAME THAT DENOTES A SUBTYPE OF THE PRIVATE TYPE, AND
     -- (3) A NAME THAT DENOTES A COMPOSITE TYPE WITH A SUBCOMPONENT 
     --     OF THE PRIVATE TYPE (OR SUBTYPE)

--  MAY NOT BE USED IN A SUBTYPE INDICATION OF AN OBJECT DECLARATION.

-- DSJ 4/27/83
-- BHS 6/15/84


PROCEDURE B74103D IS

     PACKAGE PACK1 IS

          TYPE P1 IS PRIVATE;
          TYPE LP2 IS LIMITED PRIVATE;

          SUBTYPE SP1 IS P1;
          SUBTYPE SLP2 IS LP2;

          TYPE ARR1_P1 IS ARRAY ( 1 .. 2 ) OF P1;
          TYPE ARR2_LP2 IS ARRAY ( 1 .. 2 ) OF LP2;
          TYPE ARR3_SP1 IS ARRAY ( 1 .. 2 ) OF SP1;
          TYPE ARR4_SLP2 IS ARRAY ( 1 .. 2 ) OF SLP2;

          TYPE REC1 IS
               RECORD
                    C1 : P1;
               END RECORD;

          TYPE REC2 IS
               RECORD
                    C2 : LP2;
               END RECORD;

          TYPE REC3 IS
               RECORD
                    C3 : SP1;
               END RECORD;

          TYPE REC4 IS
               RECORD
                    C4 : SLP2;
               END RECORD;

          SUBTYPE SS1 IS ARR1_P1;
          SUBTYPE SS2 IS ARR2_LP2;
          SUBTYPE SS3 IS ARR3_SP1;
          SUBTYPE SS4 IS ARR4_SLP2;
          SUBTYPE SS5 IS REC1;
          SUBTYPE SS6 IS REC2;
          SUBTYPE SS7 IS REC3;
          SUBTYPE SS8 IS REC4;

          ------------------------------------------------------------

          N01 : P1;         -- ERROR: USE OF P1
          N02 : LP2;        -- ERROR: USE OF LP2
          N03 : SP1;        -- ERROR: USE OF SP1
          N04 : SLP2;       -- ERROR: USE OF SLP2
          N05 : ARR1_P1;    -- ERROR: USE OF ARR1_P1
          N06 : ARR2_LP2;   -- ERROR: USE OF ARR2_LP2
          N07 : ARR3_SP1;   -- ERROR: USE OF ARR3_SP1
          N08 : ARR4_SLP2;  -- ERROR: USE OF ARR4_SLP2
          N09 : REC1;       -- ERROR: USE OF REC1
          N10 : REC2;       -- ERROR: USE OF REC2
          N11 : REC3;       -- ERROR: USE OF REC3
          N12 : REC4;       -- ERROR: USE OF REC4
          N13 : SS1;        -- ERROR: USE OF SS1
          N14 : SS2;        -- ERROR: USE OF SS2
          N15 : SS3;        -- ERROR: USE OF SS3
          N16 : SS4;        -- ERROR: USE OF SS4
          N17 : SS5;        -- ERROR: USE OF SS5
          N18 : SS6;        -- ERROR: USE OF SS6
          N19 : SS7;        -- ERROR: USE OF SS7
          N20 : SS8;        -- ERROR: USE OF SS8

          N21 : ARRAY (1..2) OF P1;       -- ERROR: USE OF P1
          N22 : ARRAY (1..2) OF LP2;      -- ERROR: USE OF LP2
          N23 : ARRAY (1..2) OF SP1;      -- ERROR: USE OF SP1
          N24 : ARRAY (1..2) OF SLP2;     -- ERROR: USE OF SLP2
          N25 : ARRAY (1..2) OF ARR1_P1;  -- ERROR: USE OF ARR1_P1
          N26 : ARRAY (1..2) OF ARR2_LP2; -- ERROR: USE OF ARR2_LP2
          N27 : ARRAY (1..2) OF ARR3_SP1; -- ERROR: USE OF ARR3_SP1
          N28 : ARRAY (1..2) OF ARR4_SLP2;-- ERROR: USE OF ARR4_SLP2
          N29 : ARRAY (1..2) OF REC1;     -- ERROR: USE OF REC1
          N30 : ARRAY (1..2) OF REC2;     -- ERROR: USE OF REC2
          N31 : ARRAY (1..2) OF REC3;     -- ERROR: USE OF REC3
          N32 : ARRAY (1..2) OF REC4;     -- ERROR: USE OF REC4
          N33 : ARRAY (1..2) OF SS1;      -- ERROR: USE OF SS1
          N34 : ARRAY (1..2) OF SS2;      -- ERROR: USE OF SS2
          N35 : ARRAY (1..2) OF SS3;      -- ERROR: USE OF SS3
          N36 : ARRAY (1..2) OF SS4;      -- ERROR: USE OF SS4
          N37 : ARRAY (1..2) OF SS5;      -- ERROR: USE OF SS5
          N38 : ARRAY (1..2) OF SS6;      -- ERROR: USE OF SS6
          N39 : ARRAY (1..2) OF SS7;      -- ERROR: USE OF SS7
          N40 : ARRAY (1..2) OF SS8;      -- ERROR: USE OF SS8

     PRIVATE

          PN01 : P1;           -- ERROR: USE OF P1
          PN02 : LP2;          -- ERROR: USE OF LP2
          PN03 : SP1;          -- ERROR: USE OF SP1
          PN04 : SLP2;         -- ERROR: USE OF SLP2
          PN05 : ARR1_P1;      -- ERROR: USE OF ARR1_P1
          PN06 : ARR2_LP2;     -- ERROR: USE OF ARR2_LP2
          PN07 : ARR3_SP1;     -- ERROR: USE OF ARR3_SP1
          PN08 : ARR4_SLP2;    -- ERROR: USE OF ARR4_SLP2
          PN09 : REC1;         -- ERROR: USE OF REC1
          PN10 : REC2;         -- ERROR: USE OF REC2
          PN11 : REC3;         -- ERROR: USE OF REC3
          PN12 : REC4;         -- ERROR: USE OF REC4
          PN13 : SS1;          -- ERROR: USE OF SS1
          PN14 : SS2;          -- ERROR: USE OF SS2
          PN15 : SS3;          -- ERROR: USE OF SS3
          PN16 : SS4;          -- ERROR: USE OF SS4
          PN17 : SS5;          -- ERROR: USE OF SS5
          PN18 : SS6;          -- ERROR: USE OF SS6
          PN19 : SS7;          -- ERROR: USE OF SS7
          PN20 : SS8;          -- ERROR: USE OF SS8

          PN21 : ARRAY (1..2) OF P1;       -- ERROR: USE OF P1
          PN22 : ARRAY (1..2) OF LP2;      -- ERROR: USE OF LP2
          PN23 : ARRAY (1..2) OF SP1;      -- ERROR: USE OF SP1
          PN24 : ARRAY (1..2) OF SLP2;     -- ERROR: USE OF SLP2
          PN25 : ARRAY (1..2) OF ARR1_P1;  -- ERROR: USE OF ARR1_P1
          PN26 : ARRAY (1..2) OF ARR2_LP2; -- ERROR: USE OF ARR2_LP2
          PN27 : ARRAY (1..2) OF ARR3_SP1; -- ERROR: USE OF ARR3_SP1
          PN28 : ARRAY (1..2) OF ARR4_SLP2;-- ERROR: USE OF ARR4_SLP2
          PN29 : ARRAY (1..2) OF REC1;     -- ERROR: USE OF REC1
          PN30 : ARRAY (1..2) OF REC2;     -- ERROR: USE OF REC2
          PN31 : ARRAY (1..2) OF REC3;     -- ERROR: USE OF REC3
          PN32 : ARRAY (1..2) OF REC4;     -- ERROR: USE OF REC4
          PN33 : ARRAY (1..2) OF SS1;      -- ERROR: USE OF SS1
          PN34 : ARRAY (1..2) OF SS2;      -- ERROR: USE OF SS2
          PN35 : ARRAY (1..2) OF SS3;      -- ERROR: USE OF SS3
          PN36 : ARRAY (1..2) OF SS4;      -- ERROR: USE OF SS4
          PN37 : ARRAY (1..2) OF SS5;      -- ERROR: USE OF SS5
          PN38 : ARRAY (1..2) OF SS6;      -- ERROR: USE OF SS6
          PN39 : ARRAY (1..2) OF SS7;      -- ERROR: USE OF SS7
          PN40 : ARRAY (1..2) OF SS8;      -- ERROR: USE OF SS8

          TYPE P1 IS NEW INTEGER;
          TYPE LP2 IS NEW INTEGER;

          OK01 : P1;                           -- LEGAL USE OF TYPE
          OK02 : LP2;
          OK03 : SP1;
          OK04 : SLP2;
          OK05 : ARR1_P1;
          OK06 : ARR2_LP2;
          OK07 : ARR3_SP1;
          OK08 : ARR4_SLP2;
          OK09 : REC1;
          OK10 : REC2;
          OK11 : REC3;
          OK12 : REC4;
          OK13 : SS1;
          OK14 : SS2;
          OK15 : SS3;
          OK16 : SS4;
          OK17 : SS5;
          OK18 : SS6;
          OK19 : SS7;
          OK20 : SS8;

          OK21 : ARRAY (1..2) OF P1;
          OK22 : ARRAY (1..2) OF LP2;
          OK23 : ARRAY (1..2) OF SP1;
          OK24 : ARRAY (1..2) OF SLP2;
          OK25 : ARRAY (1..2) OF ARR1_P1;
          OK26 : ARRAY (1..2) OF ARR2_LP2;
          OK27 : ARRAY (1..2) OF ARR3_SP1;
          OK28 : ARRAY (1..2) OF ARR4_SLP2;
          OK29 : ARRAY (1..2) OF REC1;
          OK30 : ARRAY (1..2) OF REC2;
          OK31 : ARRAY (1..2) OF REC3;
          OK32 : ARRAY (1..2) OF REC4;
          OK33 : ARRAY (1..2) OF SS1;
          OK34 : ARRAY (1..2) OF SS2;
          OK35 : ARRAY (1..2) OF SS3;
          OK36 : ARRAY (1..2) OF SS4;
          OK37 : ARRAY (1..2) OF SS5;
          OK38 : ARRAY (1..2) OF SS6;
          OK39 : ARRAY (1..2) OF SS7;
          OK40 : ARRAY (1..2) OF SS8;

     END PACK1;
      

BEGIN

     NULL;


END B74103D;
