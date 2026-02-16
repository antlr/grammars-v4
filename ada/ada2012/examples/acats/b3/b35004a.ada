-- B35004A.ADA

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
-- CHECK THAT THE PREFIX OF FIRST AND LAST CANNOT BE A RECORD, 
-- ACCESS, OR PRIVATE TYPE (INCLUDING A GENERIC FORMAL PRIVATE TYPE).

-- RJW 2/13/86

PROCEDURE B35004A IS
     
BEGIN
     DECLARE
          TYPE R IS RECORD
               I1, I2 : INTEGER;
          END RECORD;          
          
          REC : R;
     BEGIN
          IF (R'FIRST = REC.I1) THEN     -- ERROR: PREFIX 'R' 
                                         --        NOT VALID.
               NULL;
          ELSIF (R'LAST = REC.I2) THEN   -- ERROR: PREFIX 'R' 
                                         --        NOT VALID.
               NULL;
          END IF;
     END;

     DECLARE
          TYPE ARR IS ARRAY (1 .. 5) OF INTEGER;
          TYPE ACC IS ACCESS ARR;
     BEGIN
          IF (ACC'FIRST = 1) THEN        -- ERROR: PREFIX 'ACC'      
                                         --        NOT VALID.
               NULL;
          ELSIF (ACC'LAST = 5) THEN      -- ERROR: PREFIX 'ACC' 
                                         --        NOT VALID.
               NULL;
          END IF;
     END;     
     
     DECLARE
          PACKAGE PKG IS 
               TYPE ARR IS PRIVATE;
          PRIVATE
               TYPE ARR IS ARRAY (1 .. 5) OF INTEGER;
          END PKG;

     BEGIN
          IF (PKG.ARR'FIRST = 1) THEN    -- ERROR: PREFIX 'PKG.ARR' 
                                         --        NOT VALID.
               NULL;
          ELSIF (PKG.ARR'LAST) = 5 THEN  -- ERROR: PREFIX 'PKG.ARR' 
                                         --        NOT VALID.
               NULL;
          END IF;
     END;

     DECLARE 
          TYPE ARR IS ARRAY (1 .. 5) OF INTEGER;
          
          GENERIC
               TYPE ARR IS PRIVATE;
          PROCEDURE PROC;
  
          PROCEDURE PROC IS
          BEGIN                            
               IF ARR'FIRST = 1 THEN     -- ERROR: PREFIX 'ARR' 
                                         --        NOT VALID.
                    NULL;
               ELSIF ARR'LAST = 5 THEN   -- ERROR: PREFIX 'ARR' 
                                         --        NOT VALID.
                    NULL;
               END IF;
          END PROC;

     BEGIN
          NULL;
     END;
          
END B35004A;
