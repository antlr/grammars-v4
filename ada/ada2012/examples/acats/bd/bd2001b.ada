-- BD2001B.ADA

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
--     CHECK THAT THE SYNTAX FOR A LENGTH CLAUSE CANNOT BE USED TO
--     SPECIFY 'FIRST FOR AN INTEGER TYPE.

-- HISTORY:
--     LDC  06/14/88 CREATED ORIGINAL TEST.

PROCEDURE BD2001B IS

     FOR INTEGER'FIRST USE 0;                          -- ERROR: 'FIRST
                                                       -- INVALID

     SUBTYPE SUB_INT_TYPE IS INTEGER RANGE 16..32;
     FOR SUB_INT_TYPE'FIRST USE 16;                    -- ERROR: 'FIRST
                                                       -- INVALID

     FOR NATURAL'FIRST USE 1;                          -- ERROR: 'FIRST
                                                       -- INVALID

     SUBTYPE SUB_NAT_TYPE IS NATURAL RANGE 1..32;
     FOR SUB_NAT_TYPE'FIRST USE 1;                     -- ERROR: 'FIRST
                                                       -- INVALID

     TYPE DRV_INT_TYPE IS NEW INTEGER;
     FOR DRV_INT_TYPE'FIRST USE 2;                     -- ERROR: 'FIRST
                                                       -- INVALID

     TYPE DRV_INT_RNG_TYPE IS NEW INTEGER RANGE 1..32;
     FOR DRV_INT_RNG_TYPE'FIRST USE 1;                 -- ERROR: 'FIRST
                                                       -- INVALID

     TYPE INT_RNG_TYPE IS RANGE 1..64;
     FOR INT_RNG_TYPE'FIRST USE 1;                     -- ERROR: 'FIRST
                                                       -- INVALID
BEGIN
     NULL;
END BD2001B;
