-- B36002A.ADA

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
-- CHECK THAT THE INDICES OF MULTIDIMENSIONAL ARRAY TYPES MUST BE EITHER
-- ALL CONSTRAINED OR ALL UNCONSTRAINED.

-- L.BROWN  7/2/86

PROCEDURE B36002A IS

     TYPE ARR_INT IS ARRAY(INTEGER RANGE <>,
                     INTEGER RANGE 1 .. 2) OF INTEGER;         -- ERROR:

     TYPE NINT IS NEW INTEGER;
     TYPE ARR_NINT IS ARRAY(NINT RANGE 1 .. 4,
                      NINT RANGE <>) OF INTEGER;               -- ERROR:

     TYPE ARR_POS IS ARRAY(POSITIVE RANGE 1 .. 9,
                     POSITIVE RANGE <>) OF POSITIVE;           -- ERROR:

     TYPE ARR_CHAR IS ARRAY(CHARACTER RANGE 'A' .. 'L',
                      CHARACTER RANGE <>) OF CHARACTER;        -- ERROR:

     TYPE NCHAR IS NEW CHARACTER;
     TYPE ARR_NCHAR IS ARRAY(NCHAR RANGE 'A' .. 'D',
                       NCHAR RANGE <>) OF NCHAR;               -- ERROR:

     SUBTYPE LOWCHAR IS CHARACTER RANGE 'A' .. 'H';
     TYPE ARR_LOWCHAR IS ARRAY(LOWCHAR RANGE 'B' .. 'E',
                         LOWCHAR RANGE <>) OF CHARACTER;       -- ERROR:

     TYPE ENUM IS ('A','B','C','D',OFF,ON,'X','Y','Z');
     TYPE ARR_ENUM IS ARRAY(ENUM RANGE <>,
                      ENUM RANGE 'D' .. 'X') OF ENUM;          -- ERROR:

     SUBTYPE HIGHENUM IS ENUM RANGE 'A' .. OFF;
     TYPE ARR_HIGHENUM IS ARRAY(HIGHENUM RANGE <>,
                          HIGHENUM RANGE 'C' .. OFF) OF ENUM;  -- ERROR:

     TYPE NENUM IS NEW ENUM;
     TYPE ARR_NENUM IS ARRAY(NENUM RANGE 'A' .. 'C',
                       NENUM RANGE <>) OF ENUM;                -- ERROR:

     TYPE ARR_BOO IS ARRAY(BOOLEAN RANGE FALSE .. FALSE,
                     BOOLEAN RANGE <>) OF INTEGER;             -- ERROR:

     SUBTYPE TBOO IS BOOLEAN RANGE TRUE .. TRUE;
     TYPE ARR_TBOO IS ARRAY(TBOO RANGE TRUE .. TRUE,
                      TBOO RANGE <>) OF CHARACTER;             -- ERROR:

     TYPE NBOO IS NEW BOOLEAN;
     TYPE ARR_NBOO IS ARRAY(NBOO RANGE <>,
                      NBOO RANGE TRUE .. FALSE) OF BOOLEAN;    -- ERROR:

BEGIN
     NULL;
END B36002A;
