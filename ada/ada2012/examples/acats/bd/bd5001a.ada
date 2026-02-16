-- BD5001A.ADA

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
--     CHECK THAT THE EXPRESSION IN AN ADDRESS CLAUSE MUST HAVE THE
--     TYPE SYSTEM.ADDRESS.

-- HISTORY:
--     DJ  07/27/87  CREATED ORIGINAL TEST.
--     DWC 09/25/87  REFORMATTED TEST.

WITH SYSTEM;
PROCEDURE BD5001A IS

     SUBTYPE BAD_ADDRESS_TYPE IS INTEGER;
     BAD_ADDRESS1 : BAD_ADDRESS_TYPE := 0;
     BAD_ADDRESS2 : POSITIVE := 1;
     BAD_ADDRESS3 : INTEGER := 0;
     BAD_ADDRESS4 : NATURAL := 0;
     BAD_ADDRESS5 : FLOAT := 0.0;

     OBJECT1 : INTEGER;
     FOR OBJECT1 USE AT BAD_ADDRESS1;  -- ERROR:  ADDRESS MUST BE
                                       -- OF SYSTEM.ADDRESS.

     OBJECT2 : INTEGER;
     FOR OBJECT2 USE AT BAD_ADDRESS2;  -- ERROR:  ADDRESS MUST BE
                                       -- OF SYSTEM.ADDRESS.

     OBJECT3 : INTEGER;
     FOR OBJECT3 USE AT BAD_ADDRESS3;  -- ERROR:  ADDRESS MUST BE
                                       -- OF SYSTEM.ADDRESS.

     OBJECT4 : INTEGER;
     FOR OBJECT4 USE AT BAD_ADDRESS4;  -- ERROR:  ADDRESS MUST BE
                                       -- OF SYSTEM.ADDRESS.

     OBJECT5 : INTEGER;
     FOR OBJECT5 USE AT BAD_ADDRESS5;  -- ERROR:  ADDRESS MUST BE
                                       -- OF SYSTEM.ADDRESS.

     BEGIN
          NULL;
END BD5001A;
