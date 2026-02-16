-- B48001B.ADA

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
-- CHECK THAT ILLEGAL FORMS OF ALLOCATORS ARE FORBIDDEN. IN PARTICULAR,
-- FOR ALLOCATORS OF THE FORM "NEW T", CHECK THAT T CANNOT BE AN
-- UNCONSTRAINED ARRAY TYPE.

-- RM  11/12/80
-- RM  01/01/82
-- EG  07/05/84

PROCEDURE  B48001B  IS

BEGIN


     DECLARE   -- ARRAY TYPES

          TYPE     A1  IS  ARRAY( INTEGER RANGE <> )  OF BOOLEAN ;

          SUBTYPE  A2  IS  A1 ;
          TYPE     A3  IS  NEW A1 ;
          SUBTYPE  A4  IS  A3 ;
          TYPE     A5  IS  NEW A2 ;
 
          TYPE  AA1  IS  ACCESS A1 ;
          TYPE  AA2  IS  ACCESS A2(1 .. 6);
          TYPE  AA3  IS  ACCESS A3 ;
          TYPE  AA4  IS  ACCESS A4 ;
          TYPE  AA5  IS  ACCESS A5 ;

          W1 : AA1 ;
          W2 : AA2 ;
          W3 : AA3 ;
          W4 : AA4 ;
          W5 : AA5 ;

     BEGIN

          W1 :=  NEW  A1 ; -- ERROR: INDEX CONSTRAINT REQUIRED     
          W2 :=  NEW  A2 ; -- ERROR: INDEX CONSTRAINT REQUIRED
          W3 :=  NEW  A3 ; -- ERROR: INDEX CONSTRAINT REQUIRED
          W4 :=  NEW  A4 ; -- ERROR: INDEX CONSTRAINT REQUIRED
          W5 :=  NEW  A5 ; -- ERROR: INDEX CONSTRAINT REQUIRED

     END ;

END  B48001B;
