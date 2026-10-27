-- B84001A.ADA

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
-- CHECK THAT TASK SPECIFICATIONS AND SUBPROGRAM SPECIFICATIONS CANNOT
--     BE DESIGNATED IN  USE  CLAUSES.


-- RM 3/15/1982


PROCEDURE  B84001A  IS

     PROCEDURE  PR(X,Y:INTEGER) ;

     TASK TYPE  TT  IS
          ENTRY  A ;
     END  TT ;

     TASK  T  IS
          ENTRY  B ;
     END  T ;

     TASK BODY  TT  IS
     BEGIN
          ACCEPT  A ;
     END  TT ;

     TASK BODY  T  IS
     BEGIN
          ACCEPT  B ;
     END  T ;

     FUNCTION   FN(X,Y:INTEGER) RETURN BOOLEAN  IS
     BEGIN
          RETURN TRUE ;
     END  FN ;

     PROCEDURE  PR(X,Y:INTEGER)  IS
     BEGIN
          NULL ;
     END  PR ;


     -------------------------------------------------------------------

     PROCEDURE  USER  IS
          USE  TT ;                             -- ERROR: NOT A PACKAGE.
          USE  T ;                              -- ERROR: NOT A PACKAGE.
          USE  PR ;                             -- ERROR: NOT A PACKAGE.
          USE  FN ;                             -- ERROR: NOT A PACKAGE.
          USE  PR , FN , T , TT ;               -- ERROR: NOT PACKAGES.
     BEGIN
          NULL ;
     END  USER ;

     -------------------------------------------------------------------


BEGIN

     -------------------------------------------------------------------

     DECLARE
          USE  TT ;                             -- ERROR: NOT A PACKAGE.
          USE  T ;                              -- ERROR: NOT A PACKAGE.
          USE  PR ;                             -- ERROR: NOT A PACKAGE.
          USE  FN ;                             -- ERROR: NOT A PACKAGE.
          USE  PR , FN , T , TT ;               -- ERROR: NOT PACKAGES.
     BEGIN
          NULL ;
     END ;

     -------------------------------------------------------------------


END  B84001A ;
