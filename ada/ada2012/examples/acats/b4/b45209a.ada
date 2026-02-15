-- B45209A.ADA

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
-- CHECK THAT THE OPERANDS OF A MEMBERSHIP OPERATION MUST ALL HAVE THE
--    SAME TYPE.


-- RM 03/20/81
-- SPS 2/10/83
-- JWC 6/28/85   RENAMED FROM B45203B-AB.ADA

PROCEDURE  B45209A  IS
BEGIN

     DECLARE

          TYPE  ENUM1  IS  ( AA , BB , CC , LIT , XX , YY , ZZ );
          TYPE  ENUM2  IS  ( PP , QQ , RR );

          BOOLVAR   :  BOOLEAN ;

     BEGIN

          BOOLVAR  :=  LIT IN ENUM2 ;                 -- ERROR: BAD TYPE
          BOOLVAR  :=  LIT NOT IN ENUM2 ;             -- ERROR: BAD TYPE
          BOOLVAR  :=  LIT     IN             QQ..QQ ;-- ERROR: BAD TYPE

     END ;


     DECLARE

          TYPE  ENUM  IS  ( AA , BB , CC , LIT , XX , YY , ZZ );
          TYPE  NEWENUM   IS  NEW ENUM RANGE AA..YY ;

          VAR  :           NEWENUM  :=  LIT ;
          CON  :  CONSTANT ENUM     :=  LIT ;

          BOOLVAR   :  BOOLEAN ;

     BEGIN

          BOOLVAR  :=  CON     IN NEWENUM ;           -- ERROR: BAD TYPE
          BOOLVAR  :=  VAR NOT IN ENUM ;              -- ERROR: BAD TYPE
          BOOLVAR  :=  CON     IN NEWENUM'(YY) ..AA ; -- ERROR: BAD TYPE

     END ;


END B45209A ;
