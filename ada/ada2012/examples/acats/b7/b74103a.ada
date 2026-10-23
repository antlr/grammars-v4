-- B74103A.ADA

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
-- CHECK THAT BEFORE THE FULL DECLARATION OF A PRIVATE TYPE, THE
-- FOLLOWING NAMES MAY NOT BE USED IN A DERIVED TYPE DEFINITION

     -- (1) THE NAME OF THE PRIVATE TYPE,
     -- (2) A NAME THAT DENOTES A SUBTYPE OF THE PRIVATE TYPE,
     -- (3) A NAME THAT DENOTES A COMPOSITE TYPE WITH A SUBCOMPONENT 
     --     OF THE PRIVATE TYPE (OR SUBTYPE).


-- DSJ 4/27/83
-- BHS 6/12/84
-- JRK 12/4/84


PROCEDURE B74103A IS

     PACKAGE PACK1 IS

          TYPE P1 IS PRIVATE;
          TYPE LP2 IS LIMITED PRIVATE;

          SUBTYPE SP1 IS P1;
          SUBTYPE SLP2 IS LP2;

          TYPE ARR1_P1 IS ARRAY ( 1 .. 2 ) OF P1;
          TYPE ARR2_LP2 IS ARRAY ( 1 .. 2 ) OF LP2;
          TYPE ARR3_SP1 IS ARRAY ( 1 .. 2 ) OF SP1;
          TYPE ARR4_SLP2 IS ARRAY ( 1 .. 2 ) OF SLP2;
          TYPE ARR51 IS ARRAY ( 1 .. 2 ) OF ARR1_P1;
          TYPE ARR52 IS ARRAY ( 1 .. 2 ) OF ARR2_LP2;
          TYPE ARR53 IS ARRAY ( 1 .. 2 ) OF ARR3_SP1;
          TYPE ARR54 IS ARRAY ( 1 .. 2 ) OF ARR4_SLP2;

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

          TYPE REC51 IS
               RECORD
                    C5 : REC1;
               END RECORD;

          TYPE REC52 IS
               RECORD
                    C6 : REC2;
               END RECORD;
          
          TYPE REC53 IS
               RECORD
                    C7 : REC3;
               END RECORD;

          TYPE REC54 IS
               RECORD
                    C8 : REC4;
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

          TYPE N01 IS NEW P1;         -- ERROR: USE OF P1
          TYPE N02 IS NEW LP2;        -- ERROR: USE OF LP2
          TYPE N03 IS NEW SP1;        -- ERROR: USE OF SP1
          TYPE N04 IS NEW SLP2;       -- ERROR: USE OF SLP2
          TYPE N05 IS NEW ARR1_P1;    -- ERROR: USE OF ARR1_P1
          TYPE N06 IS NEW ARR2_LP2;   -- ERROR: USE OF ARR2_LP2
          TYPE N07 IS NEW ARR3_SP1;   -- ERROR: USE OF ARR3_SP1
          TYPE N08 IS NEW ARR4_SLP2;  -- ERROR: USE OF ARR4_SLP2
          TYPE N09 IS NEW REC1;       -- ERROR: USE OF REC1
          TYPE N10 IS NEW REC2;       -- ERROR: USE OF REC2
          TYPE N11 IS NEW REC3;       -- ERROR: USE OF REC3
          TYPE N12 IS NEW REC4;       -- ERROR: USE OF REC4
          TYPE N13 IS NEW SS1;        -- ERROR: USE OF SS1
          TYPE N14 IS NEW SS2;        -- ERROR: USE OF SS2
          TYPE N15 IS NEW SS3;        -- ERROR: USE OF SS3
          TYPE N16 IS NEW SS4;        -- ERROR: USE OF SS4
          TYPE N17 IS NEW SS5;        -- ERROR: USE OF SS5
          TYPE N18 IS NEW SS6;        -- ERROR: USE OF SS6
          TYPE N19 IS NEW SS7;        -- ERROR: USE OF SS7
          TYPE N20 IS NEW SS8;        -- ERROR: USE OF SS8
          TYPE N21 IS NEW REC51;      -- ERROR: USE OF REC51
          TYPE N22 IS NEW REC52;      -- ERROR: USE OF REC52
          TYPE N23 IS NEW REC53;      -- ERROR: USE OF REC53
          TYPE N24 IS NEW REC54;      -- ERROR: USE OF REC54
          TYPE N25 IS NEW ARR51;      -- ERROR: USE OF ARR51
          TYPE N26 IS NEW ARR52;      -- ERROR: USE OF ARR52
          TYPE N27 IS NEW ARR53;      -- ERROR: USE OF ARR53
          TYPE N28 IS NEW ARR54;      -- ERROR: USE OF ARR54

     PRIVATE

          TYPE PN01 IS NEW P1;          -- ERROR: USE OF P1
          TYPE PN02 IS NEW LP2;         -- ERROR: USE OF LP2
          TYPE PN03 IS NEW SP1;         -- ERROR: USE OF SP1
          TYPE PN04 IS NEW SLP2;        -- ERROR: USE OF SLP2
          TYPE PN05 IS NEW ARR1_P1;     -- ERROR: USE OF ARR1_P1
          TYPE PN06 IS NEW ARR2_LP2;    -- ERROR: USE OF ARR2_LP2
          TYPE PN07 IS NEW ARR3_SP1;    -- ERROR: USE OF ARR3_SP1
          TYPE PN08 IS NEW ARR4_SLP2;   -- ERROR: USE OF ARR4_SLP2
          TYPE PN09 IS NEW REC1;        -- ERROR: USE OF REC1
          TYPE PN10 IS NEW REC2;        -- ERROR: USE OF REC2
          TYPE PN11 IS NEW REC3;        -- ERROR: USE OF REC3
          TYPE PN12 IS NEW REC4;        -- ERROR: USE OF REC4
          TYPE PN13 IS NEW SS1;         -- ERROR: USE OF SS1
          TYPE PN14 IS NEW SS2;         -- ERROR: USE OF SS2
          TYPE PN15 IS NEW SS3;         -- ERROR: USE OF SS3
          TYPE PN16 IS NEW SS4;         -- ERROR: USE OF SS4
          TYPE PN17 IS NEW SS5;         -- ERROR: USE OF SS5
          TYPE PN18 IS NEW SS6;         -- ERROR: USE OF SS6
          TYPE PN19 IS NEW SS7;         -- ERROR: USE OF SS7
          TYPE PN20 IS NEW SS8;         -- ERROR: USE OF SS8
          TYPE PN21 IS NEW REC51;       -- ERROR: USE OF REC51
          TYPE PN22 IS NEW REC52;       -- ERROR: USE OF REC52
          TYPE PN23 IS NEW REC53;       -- ERROR: USE OF REC53
          TYPE PN24 IS NEW REC54;       -- ERROR: USE OF REC54
          TYPE PN25 IS NEW ARR51;       -- ERROR: USE OF ARR51
          TYPE PN26 IS NEW ARR52;       -- ERROR: USE OF ARR52
          TYPE PN27 IS NEW ARR53;       -- ERROR: USE OF ARR53
          TYPE PN28 IS NEW ARR54;       -- ERROR: USE OF ARR54


          TYPE P1  IS NEW INTEGER;
          TYPE LP2 IS NEW INTEGER;


          TYPE OK01 IS NEW P1;              -- LEGAL USE OF P1
          TYPE OK02 IS NEW LP2;             -- LEGAL USE OF LP2
          TYPE OK03 IS NEW SP1;             -- LEGAL USE OF SP1
          TYPE OK04 IS NEW SLP2;            -- LEGAL USE OF SLP2
          TYPE OK05 IS NEW ARR1_P1;         -- LEGAL USE OF ARR1_P1
          TYPE OK06 IS NEW ARR2_LP2;        -- LEGAL USE OF ARR2_LP2
          TYPE OK07 IS NEW ARR3_SP1;        -- LEGAL USE OF ARR3_SP1
          TYPE OK08 IS NEW ARR4_SLP2;       -- LEGAL USE OF ARR4_SLP2
          TYPE OK09 IS NEW REC1;            -- LEGAL USE OF REC1
          TYPE OK10 IS NEW REC2;            -- LEGAL USE OF REC2
          TYPE OK11 IS NEW REC3;            -- LEGAL USE OF REC3
          TYPE OK12 IS NEW REC4;            -- LEGAL USE OF REC4
          TYPE OK13 IS NEW SS1;             -- LEGAL USE OF SS1
          TYPE OK14 IS NEW SS2;             -- LEGAL USE OF SS2
          TYPE OK15 IS NEW SS3;             -- LEGAL USE OF SS3
          TYPE OK16 IS NEW SS4;             -- LEGAL USE OF SS4
          TYPE OK17 IS NEW SS5;             -- LEGAL USE OF SS5
          TYPE OK18 IS NEW SS6;             -- LEGAL USE OF SS6
          TYPE OK19 IS NEW SS7;             -- LEGAL USE OF SS7
          TYPE OK20 IS NEW SS8;             -- LEGAL USE OF SS8
          TYPE OK21 IS NEW REC51;           -- LEGAL USE OF REC51
          TYPE OK22 IS NEW REC52;           -- LEGAL USE OF REC52
          TYPE OK23 IS NEW REC53;           -- LEGAL USE OF REC53
          TYPE OK24 IS NEW REC54;           -- LEGAL USE OF REC54
          TYPE OK25 IS NEW ARR51;           -- LEGAL USE OF ARR51
          TYPE OK26 IS NEW ARR52;           -- LEGAL USE OF ARR52
          TYPE OK27 IS NEW ARR53;           -- LEGAL USE OF ARR53
          TYPE OK28 IS NEW ARR54;           -- LEGAL USE OF ARR54

     END PACK1;


BEGIN

     NULL;


END B74103A;
