-- B74103G.ADA

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
-- FOLLOWING NAMES MAY NOT BE USED IN AN ALLOCATOR :

     -- (1) THE NAME OF THE PRIVATE TYPE,
     -- (2) A NAME THAT DENOTES A SUBTYPE OF THE PRIVATE TYPE, AND
     -- (3) A NAME THAT DENOTES A COMPOSITE TYPE WITH A SUBCOMPONENT 
     --     OF THE PRIVATE TYPE (OR SUBTYPE).


-- BHS 6/13/84


PROCEDURE B74103G IS

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

          TYPE AC_P1 IS ACCESS P1;
          TYPE AC_LP2 IS ACCESS LP2;
          TYPE AC_SP1 IS ACCESS SP1;
          TYPE AC_SLP2 IS ACCESS SLP2;
          TYPE AC_ARR1 IS ACCESS ARR1_P1;
          TYPE AC_ARR2 IS ACCESS ARR2_LP2;
          TYPE AC_ARR3 IS ACCESS ARR3_SP1;
          TYPE AC_ARR4 IS ACCESS ARR4_SLP2;
          TYPE AC_REC1 IS ACCESS REC1;
          TYPE AC_REC2 IS ACCESS REC2;
          TYPE AC_REC3 IS ACCESS REC3;
          TYPE AC_REC4 IS ACCESS REC4;
          TYPE AC_SS1 IS ACCESS SS1;
          TYPE AC_SS2 IS ACCESS SS2;
          TYPE AC_SS3 IS ACCESS SS3;
          TYPE AC_SS4 IS ACCESS SS4;
          TYPE AC_SS5 IS ACCESS SS5;
          TYPE AC_SS6 IS ACCESS SS6;
          TYPE AC_SS7 IS ACCESS SS7;
          TYPE AC_SS8 IS ACCESS SS8;
          TYPE AC_ARR51 IS ACCESS ARR51;
          TYPE AC_ARR52 IS ACCESS ARR52;
          TYPE AC_ARR53 IS ACCESS ARR53;
          TYPE AC_ARR54 IS ACCESS ARR54;
          TYPE AC_REC51 IS ACCESS REC51;
          TYPE AC_REC52 IS ACCESS REC52;
          TYPE AC_REC53 IS ACCESS REC53;
          TYPE AC_REC54 IS ACCESS REC54;

          --------------------------------------------------------

          N01 : AC_P1 := NEW P1;             -- ERROR: USE OF P1
          N02 : AC_LP2 := NEW LP2;           -- ERROR: USE OF LP2
          N03 : AC_SP1 := NEW SP1;           -- ERROR: USE OF SP1
          N04 : AC_SLP2 := NEW SLP2;         -- ERROR: USE OF SLP2
          N05 : AC_ARR1 := NEW ARR1_P1;      -- ERROR: USE OF ARR1_P1
          N06 : AC_ARR2 := NEW ARR2_LP2;     -- ERROR: USE OF ARR2_LP2
          N07 : AC_ARR3 := NEW ARR3_SP1;     -- ERROR: USE OF ARR3_SP1
          N08 : AC_ARR4 := NEW ARR4_SLP2;    -- ERROR: USE OF ARR4_SLP2
          N09 : AC_REC1 := NEW REC1;         -- ERROR: USE OF REC1
          N10 : AC_REC2 := NEW REC2;         -- ERROR: USE OF REC2
          N11 : AC_REC3 := NEW REC3;         -- ERROR: USE OF REC3
          N12 : AC_REC4 := NEW REC4;         -- ERROR: USE OF REC4
          N13 : AC_SS1  := NEW SS1;          -- ERROR: USE OF SS1
          N14 : AC_SS2  := NEW SS2;          -- ERROR: USE OF SS2
          N15 : AC_SS3  := NEW SS3;          -- ERROR: USE OF SS3
          N16 : AC_SS4  := NEW SS4;          -- ERROR: USE OF SS4
          N17 : AC_SS5  := NEW SS5;          -- ERROR: USE OF SS5
          N18 : AC_SS6  := NEW SS6;          -- ERROR: USE OF SS6
          N19 : AC_SS7  := NEW SS7;          -- ERROR: USE OF SS7
          N20 : AC_SS8  := NEW SS8;          -- ERROR: USE OF SS8
          N21 : AC_ARR51 := NEW ARR51;       -- ERROR: USE OF ARR51
          N22 : AC_ARR52 := NEW ARR52;       -- ERROR: USE OF ARR52
          N23 : AC_ARR53 := NEW ARR53;       -- ERROR: USE OF ARR53
          N24 : AC_ARR54 := NEW ARR54;       -- ERROR: USE OF ARR54
          N25 : AC_REC51 := NEW REC51;       -- ERROR: USE OF REC51
          N26 : AC_REC52 := NEW REC52;       -- ERROR: USE OF REC52
          N27 : AC_REC53 := NEW REC53;       -- ERROR: USE OF REC53
          N28 : AC_REC54 := NEW REC54;       -- ERROR: USE OF REC54

      PRIVATE

          PN01 : AC_P1 := NEW P1;             -- ERROR: USE OF P1
          PN02 : AC_LP2 := NEW LP2;           -- ERROR: USE OF LP2
          PN03 : AC_SP1 := NEW SP1;           -- ERROR: USE OF SP1
          PN04 : AC_SLP2 := NEW SLP2;         -- ERROR: USE OF SLP2
          PN05 : AC_ARR1 := NEW ARR1_P1;      -- ERROR: USE OF ARR1_P1
          PN06 : AC_ARR2 := NEW ARR2_LP2;     -- ERROR: USE OF ARR2_LP2
          PN07 : AC_ARR3 := NEW ARR3_SP1;     -- ERROR: USE OF ARR3_SP1
          PN08 : AC_ARR4 := NEW ARR4_SLP2;    -- ERROR: USE OF ARR4_SLP2
          PN09 : AC_REC1 := NEW REC1;         -- ERROR: USE OF REC1
          PN10 : AC_REC2 := NEW REC2;         -- ERROR: USE OF REC2
          PN11 : AC_REC3 := NEW REC3;         -- ERROR: USE OF REC3
          PN12 : AC_REC4 := NEW REC4;         -- ERROR: USE OF REC4
          PN13 : AC_SS1  := NEW SS1;          -- ERROR: USE OF SS1
          PN14 : AC_SS2  := NEW SS2;          -- ERROR: USE OF SS2
          PN15 : AC_SS3  := NEW SS3;          -- ERROR: USE OF SS3
          PN16 : AC_SS4  := NEW SS4;          -- ERROR: USE OF SS4
          PN17 : AC_SS5  := NEW SS5;          -- ERROR: USE OF SS5
          PN18 : AC_SS6  := NEW SS6;          -- ERROR: USE OF SS6
          PN19 : AC_SS7  := NEW SS7;          -- ERROR: USE OF SS7
          PN20 : AC_SS8  := NEW SS8;          -- ERROR: USE OF SS8
          PN21 : AC_ARR51 := NEW ARR51;       -- ERROR: USE OF ARR51
          PN22 : AC_ARR52 := NEW ARR52;       -- ERROR: USE OF ARR52
          PN23 : AC_ARR53 := NEW ARR53;       -- ERROR: USE OF ARR53
          PN24 : AC_ARR54 := NEW ARR54;       -- ERROR: USE OF ARR54
          PN25 : AC_REC51 := NEW REC51;       -- ERROR: USE OF REC51
          PN26 : AC_REC52 := NEW REC52;       -- ERROR: USE OF REC52
          PN27 : AC_REC53 := NEW REC53;       -- ERROR: USE OF REC53
          PN28 : AC_REC54 := NEW REC54;       -- ERROR: USE OF REC54

          TYPE P1 IS NEW INTEGER;
          TYPE LP2 IS NEW INTEGER;

          OK01 : AC_P1 := NEW P1;             -- LEGAL USE OF TYPE
          OK02 : AC_LP2 := NEW LP2;
          OK03 : AC_SP1 := NEW SP1;
          OK04 : AC_SLP2 := NEW SLP2;
          OK05 : AC_ARR1 := NEW ARR1_P1;
          OK06 : AC_ARR2 := NEW ARR2_LP2;
          OK07 : AC_ARR3 := NEW ARR3_SP1;
          OK08 : AC_ARR4 := NEW ARR4_SLP2;
          OK09 : AC_REC1 := NEW REC1;
          OK10 : AC_REC2 := NEW REC2;
          OK11 : AC_REC3 := NEW REC3;
          OK12 : AC_REC4 := NEW REC4;
          OK13 : AC_SS1 := NEW SS1;
          OK14 : AC_SS2 := NEW SS2;
          OK15 : AC_SS3 := NEW SS3;
          OK16 : AC_SS4 := NEW SS4;
          OK17 : AC_SS5 := NEW SS5;
          OK18 : AC_SS6 := NEW SS6;
          OK19 : AC_SS7 := NEW SS7;
          OK20 : AC_SS8 := NEW SS8;
          OK21 : AC_ARR51 := NEW ARR51;
          OK22 : AC_ARR52 := NEW ARR52;
          OK23 : AC_ARR53 := NEW ARR53;
          OK24 : AC_ARR54 := NEW ARR54;
          OK25 : AC_REC51 := NEW REC51;
          OK26 : AC_REC52 := NEW REC52;
          OK27 : AC_REC53 := NEW REC53;
          OK28 : AC_REC54 := NEW REC54;

     END PACK1;


BEGIN

     NULL;


END B74103G;
