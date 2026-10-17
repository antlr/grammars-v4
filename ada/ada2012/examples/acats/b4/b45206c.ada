-- B45206C.ADA

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
-- CHECK THAT THE RELATIONAL OPERATORS ARE NOT PREDEFINED FOR OPERANDS
-- OF DIFFERENT TYPES.


-- THIS OBJECTIVE DOES NOT DEAL WITH MIXED-MODE COMPARISONS WHICH "MAKE
-- NO SENSE"; IT LIMITS ITSELF TO COMPARISONS WHICH ARE LEGAL IN
-- CERTAIN PROGRAMMING LANGUAGES.


-- CASES COVERED:

--   * NUMERIC (INTEGER) VS. BOOLEAN
--   * NUMERIC (INTEGER) VS. ARRAY OF BOOLEANS
--   * NUMERIC (INTEGER) VS. CHARACTER
--   * NUMERIC (INTEGER) VS. ARRAY OF INTEGERS
--   * USER-DEFINED ENUMERATION VS. CHARACTER
--   * USER-DEFINED ENUMERATION VS. INTEGER  
--   * NUMERIC (INTEGER) VS. ADDRESS


-- JRL 03/16/92 CONSOLIDATED B45206A AND B45206B; COMPRESSED CODE.

WITH SYSTEM ; USE SYSTEM ;

PROCEDURE B45206C IS

BEGIN

   ---------------------------------------------------------------------
   ----------------  NUMERIC (INTEGER) VS. BOOLEAN  --------------------

   DECLARE
      INT  : INTEGER := 0 ;
      BOOL : BOOLEAN := FALSE ;
   BEGIN
      IF ( BOOL = 0 )               -- ERROR: BAD RELATIONAL EXPRESSION.
      THEN
         BOOL := ( INT /= FALSE ) ; -- ERROR: BAD RELATIONAL EXPRESSION.
      END IF ;
   END ;


   ---------------------------------------------------------------------
   -------------  NUMERIC (INTEGER) VS. ARRAY OF BOOLEANS  -------------

   DECLARE
      INT      : INTEGER := 0 ;
      BOOL_ARR : ARRAY ( CHARACTER ) OF BOOLEAN := ( OTHERS => FALSE ) ;
      BOOL     : BOOLEAN ;
   BEGIN
      BOOL := (BOOL_ARR = FALSE) ;  -- ERROR: BAD RELATIONAL EXPRESSION
                                    --       (SINGLE BOOLEAN VS. ARRAY).
      IF ( BOOL_ARR /= 0 )          -- ERROR: BAD RELATIONAL EXPRESSION.
      THEN
         BOOL := (INT > BOOL_ARR) ; -- ERROR: BAD RELATIONAL EXPRESSION.
      END IF;
   END ;


   ---------------------------------------------------------------------
   ---------------  NUMERIC (INTEGER) VS. CHARACTER  -------------------
                     
   DECLARE
      INT  :  INTEGER   RANGE  0 .. 127  := 67  ; -- HEX 43 ('C')
      CHAR :  CHARACTER RANGE 'A' .. 'D' := 'C' ;
      BOOL :  BOOLEAN ;
   BEGIN
      BOOL := ( 'C' /= 67 ) ;       -- ERROR: BAD RELATIONAL EXPRESSION.
      IF ( CHAR < 67 )              -- ERROR: BAD RELATIONAL EXPRESSION.
      THEN
         BOOL := ( INT  <= 'C' ) ;  -- ERROR: BAD RELATIONAL EXPRESSION.
         BOOL := ( CHAR >= INT ) ;  -- ERROR: BAD RELATIONAL EXPRESSION.
      END IF;
   END ;


   ---------------------------------------------------------------------
   --------------- NUMERIC (INTEGER) VS. ARRAY OF INTEGERS  ------------
                            
   DECLARE
      INT     : INTEGER RANGE 0 .. 127 := 0 ;
      INT_ARR : ARRAY ( CHARACTER ) OF INTEGER := ( CHARACTER => 0 ) ;
      BOOL    : BOOLEAN ;
   BEGIN
      IF ( INT_ARR > INT )          -- ERROR: BAD RELATIONAL EXPRESSION.
      THEN
         BOOL := ( INT_ARR = 0 ) ;  -- ERROR: BAD RELATIONAL EXPRESSION.
      END IF;
   END ;


   ---------------------------------------------------------------------
   ------------   USER-DEFINED ENUMERATION VS. CHARACTER  --------------

   DECLARE
      TYPE ENUM_TYPE IS ( AAA , BBB , 'C' , DDD , 'E' ) ;
      CHAR    :          CHARACTER := 'D' ;
      CHARCON : CONSTANT CHARACTER := 'D' ;
      ENUM    :          ENUM_TYPE := DDD ;
      ENUMCON : CONSTANT ENUM_TYPE := DDD ;
      BOOL    :          BOOLEAN ;
   BEGIN
      BOOL := (CHARCON > ENUMCON) ; -- ERROR: BAD RELATIONAL EXPRESSION.
      IF ( ENUM <= 'D' )            -- ERROR: BAD RELATIONAL EXPRESSION.
      THEN
         BOOL := ( CHAR /= ENUM ) ; -- ERROR: BAD RELATIONAL EXPRESSION.
         BOOL := (ENUMCON < 'D') ;  -- ERROR: BAD RELATIONAL EXPRESSION.
      END IF ;
   END ;


   ---------------------------------------------------------------------
   --------------   USER-DEFINED ENUMERATION VS. INTEGER  --------------

   DECLARE
      TYPE ENUM_TYPE IS ( AAA , BBB , 'C' , DDD , 'E' ) ;
      INTCON  : CONSTANT INTEGER   := 3 ;   -- POSITION NUMBER OF DDD.
      INT     :          INTEGER   := 3 ;
      ENUMCON : CONSTANT ENUM_TYPE := DDD ;
      ENUM    :          ENUM_TYPE := DDD ;
      BOOL    :          BOOLEAN   ;
   BEGIN
      BOOL := ( INT <= ENUM ) ;     -- ERROR: BAD RELATIONAL EXPRESSION.
      BOOL := ( ENUMCON < 3 ) ;     -- ERROR: BAD RELATIONAL EXPRESSION.
      BOOL := (INTCON >= ENUMCON) ; -- ERROR: BAD RELATIONAL EXPRESSION.
      IF ( ENUM >= 3 )              -- ERROR: BAD RELATIONAL EXPRESSION.
      THEN
         BOOL := ( DDD < 3 ) ;      -- ERROR: BAD RELATIONAL EXPRESSION.
      END IF ;
   END ;


   ---------------------------------------------------------------------
   ------------------  NUMERIC (INTEGER) VS. ADDRESS -------------------

   DECLARE
      INT  : INTEGER := 0 ;
      BOOL : BOOLEAN := FALSE ;
   BEGIN
      BOOL := (INT = BOOL'ADDRESS); -- ERROR: BAD RELATIONAL EXPRESSION.
   END ;


   ---------------------------------------------------------------------

END B45206C ;
