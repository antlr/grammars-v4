-- BC3502C.ADA

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
-- CHECK THAT WHEN A GENERIC FORMAL TYPE IS AN ACCESS TYPE, THE FORMAL
-- TYPE IS ONLY MATCHED WHEN ITS DESIGNATED BASE TYPE IS THE SAME AS
-- THE DESIGNATED TYPE OF THE ACTUAL PARAMETER.

-- CHECK FOR WHEN THE DESIGNATED TYPE IS NOT A GENERIC FORMAL TYPE.

-- CHECK FOR WHEN THE DESIGNATED TYPE IS A RECORD TYPE.

-- SPS 5/25/82
-- JRL 10/19/96 Removed cases under discussion in AI-00034.

PROCEDURE BC3502C IS

     SUBTYPE CC IS INTEGER RANGE 1 .. 10;
     TYPE R1 IS RECORD NULL; END RECORD;
     TYPE R2 IS
     RECORD
          C1 : INTEGER;
          C2 : CHARACTER;
          C3 : STRING(1 .. 10);
          C4 : R1;
     END RECORD;
     TYPE R3 (D : CC) IS
     RECORD
          CASE D IS
               WHEN 1 .. 3 => C1 : INTEGER;
               WHEN OTHERS => C2 : CHARACTER;
          END CASE;
     END RECORD;
     TYPE NR1 IS NEW R1;
     TYPE R5 IS RECORD NULL; END RECORD;
     TYPE NR2 IS NEW R2;
     TYPE NR3 IS NEW R3;
     TYPE R8 (D: CC) IS
     RECORD
          CASE D IS
               WHEN 1 .. 3  => C1 : INTEGER;
               WHEN OTHERS => C2 : CHARACTER;
          END CASE;
     END RECORD;
     SUBTYPE CR3 IS R3 (D => 3);
     SUBTYPE CR8 IS R8 (D => 3);

     TYPE AR1 IS ACCESS R1;
     TYPE AR2 IS ACCESS R2;
     TYPE AR3 IS ACCESS R3;
     TYPE ANR1 IS ACCESS NR1;
     TYPE AR5 IS ACCESS R5;
     TYPE ANR2 IS ACCESS NR2;
     TYPE ANR3 IS ACCESS NR3;
     TYPE AR8 IS ACCESS R8;
     TYPE ACR3 IS ACCESS CR3;
     TYPE ACR8 IS ACCESS CR8;
     TYPE AR3C IS ACCESS R3 (D => 3);
     TYPE AR8C IS ACCESS R8 (D => 3);
     SUBTYPE CAR8 IS AR8 (D => 3);

     GENERIC
          TYPE FT IS ACCESS R1;
     PACKAGE PR1 IS END PR1;

     GENERIC
          TYPE FT IS ACCESS R2;
     PACKAGE PR2 IS END PR2;

     GENERIC
          TYPE FT IS ACCESS R3;
     PACKAGE PR3 IS END PR3;

     GENERIC
          TYPE FT IS ACCESS R8;
     PACKAGE PR8 IS END PR8;

     GENERIC
          TYPE FT IS ACCESS CR3;
     PACKAGE PCR3 IS END PCR3;

     PACKAGE PR11 IS NEW PR1 (AR1);          -- OK.
     PACKAGE PR12 IS NEW PR1 (AR2);          -- ERROR: AR2.
     PACKAGE PR13 IS NEW PR1 (AR3);          -- ERROR: AR3.
     PACKAGE PR14 IS NEW PR1 (ANR1);         -- ERROR: ANR1.
     PACKAGE PR15 IS NEW PR1 (AR5);          -- ERROR: AR5.

     PACKAGE PR21 IS NEW PR2 (AR1);          -- ERROR: AR1.
     PACKAGE PR22 IS NEW PR2 (AR2);          -- OK.
     PACKAGE PR23 IS NEW PR2 (AR3);          -- ERROR: AR3.
     PACKAGE PR24 IS NEW PR2 (ANR2);         -- ERROR: ANR2.

     PACKAGE PR31 IS NEW PR3 (AR3);          -- OK.
     PACKAGE PR32 IS NEW PR3 (ANR3);         -- ERROR: ANR3.
     PACKAGE PR33 IS NEW PR3 (AR8);          -- ERROR: AR8.
     PACKAGE PR34 IS NEW PR3 (AR1);          -- ERROR: AR1.
     PACKAGE PR35 IS NEW PR3 (AR2);          -- ERROR: AR2.

     PACKAGE PR81 IS NEW PR8 (AR8);          -- OK.
     PACKAGE PR82 IS NEW PR8 (AR3);          -- ERROR: AR3.
     PACKAGE PR83 IS NEW PR8 (AR1);          -- ERROR: AR1.

     PACKAGE PCR31 IS NEW PCR3 (ACR3);       -- OK.
     PACKAGE PCR33 IS NEW PCR3 (AR3C);       -- OK.
     PACKAGE PCR34 IS NEW PCR3 (ACR8);       -- ERROR: ACR8.
     PACKAGE PCR35 IS NEW PCR3 (CAR8);       -- ERROR: CAR8.
     PACKAGE PCR36 IS NEW PCR3 (AR8C);       -- ERROR: AR8C.

BEGIN
     NULL;
END BC3502C;
