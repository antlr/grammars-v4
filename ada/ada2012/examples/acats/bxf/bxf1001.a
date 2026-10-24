-- BXF1001.A
--
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
--
-- OBJECTIVE:
--      Check that values of 2 and 10 are allowable values for Machine_Radix
--      of a decimal first subtype.  Check that values other than 2 and 10
--      are not allowed for Machine_Radix of a decimal first subtype.
--      Check that the expression used to define Machine_Radix must be static.
--      
--      Check that the package Ada.Decimal is available.  Check that
--      10**(-Max_Scale) is allowed as a decimal type's delta. Check
--      that 10**(-Min_Scale) is allowed as a decimal type's delta.
--      Check that Min_Delta and Max_Delta are allowed for delta in 
--      decimal fixed point definitions.  Check that Max_Decimal_Digits 
--      is allowed for digits in a decimal fixed point definition.
--
--      Check that a value N larger than Max_Scale is not allowed in the 
--      expression 10**(-N) as a decimal type's delta. Check that a value 
--      N smaller than Min_Scale is not allowed in the expression 10**(-N) 
--      as a decimal type's delta.  Check that neither a value smaller than 
--      Min_Delta nor a value larger than Max_Delta are allowed for delta in 
--      decimal fixed point definitions.  Check that a value larger than 
--      Max_Decimal_Digits is not allowed for digits in a decimal fixed point 
--      definition.  
--
-- TEST DESCRIPTION:
--      This test demonstrates that the Machine_Radix of a decimal
--      first subtype can be set to either 2 or 10, using static expressions
--      in an attribute definition clause. Various decimal types are defined
--      and the value of Machine_Radix specified.  Attempts to set the
--      Machine_Radix value to other than 2 or 10, or to use a non-static
--      expression to do so, should be rejected by the compiler.
--      In addition, this test examines the constants defined in package
--      Ada.Decimal.  They are available for use in the definition of
--      decimal types, defining the maximum and minimum values available
--      for specifying delta and digits values.  Attempts to use values 
--      that are beyond these ranges in defining decimal types should be
--      rejected by the compiler.
--
--       
-- CHANGE HISTORY:
--      06 Dec 94   SAIC    ACVC 2.0
--
--!

with Report;       -- to allow use of Ident_Int for non-static expression.
with Ada.Decimal;

procedure BXF1001 is
begin

   Radix_Block:
   declare

      Byte         : constant := 2*4;
      Const_Digits : constant := 3;
      Del          : constant := 1.0/10**2;
      Hex          : constant := 16#FF#;   -- value 255.
      Ten          : constant := 10;

      type Radix_Type_1  is delta 0.01  digits 7;
      type Radix_Type_2  is delta 0.001 digits 10+5;
      type Radix_Type_3  is delta Del   digits 7  range 0.0 .. 1.0E4;
      type Radix_Type_4  is delta 0.1   digits Const_Digits;
      type Radix_Type_5  is delta 0.01  digits 10 range -1.0 .. 1.0;
      type Radix_Type_6  is delta 0.01  digits Byte;
      type Radix_Type_7  is delta 0.001 digits 2*Byte;
      type Radix_Type_8  is delta Del   digits 15;
      type Radix_Type_9  is delta 0.1   digits Const_Digits*Byte/4;
      type Radix_Type_10 is delta 0.01  digits Ten range -10.0 .. 10.0;

      -- Use an attribute definition clause to set the Machine_Radix for a
      -- decimal first subtype to either 2 or 10.
      for Radix_Type_1'Machine_Radix use 2;                       -- OK.
      for Radix_Type_2'Machine_Radix use Ten;                     -- OK.
      for Radix_Type_3'Machine_Radix use 10-8;                    -- OK.
      for Radix_Type_4'Machine_Radix use 1+9;                     -- OK.
      for Radix_Type_5'Machine_Radix use 2*15/Const_Digits;       -- OK.

      -- Values other than 2 or 10, or non-static expressions, must be
      -- rejected.
      for Radix_Type_6'Machine_Radix use 1;                       -- ERROR:
                                                      -- Value not 2 or 10.
      for Radix_Type_7'Machine_Radix use 10-2;                    -- ERROR:
                                                      -- Value not 2 or 10.
      for Radix_Type_8'Machine_Radix use Report.Ident_Int(2);     -- ERROR:
                                                  -- Non-static expression.
      for Radix_Type_9'Machine_Radix use 16;                      -- ERROR:
                                                      -- Value not 2 or 10.
      for Radix_Type_10'Machine_Radix use Hex;                    -- ERROR:
                                                      -- Value not 2 or 10.
   begin
      null;
   end Radix_Block;


   Decimal_Block:
   declare

      -- Use constants Max_Scale and Min_Scale in the definition of a
      -- decimal type's delta.
      type Dec_Type_1 is delta 10.0**(-Ada.Decimal.Max_Scale) digits 8; -- OK.
      type Dec_Type_2 is delta 10.0**(-Ada.Decimal.Min_Scale) digits 6; -- OK.

      -- Use constants Max_Delta and Min_Delta in the definition of a
      -- decimal type's delta.
      type Dec_Type_3 is delta Ada.Decimal.Min_Delta digits 10;         -- OK.
      type Dec_Type_4 is delta Ada.Decimal.Max_Delta digits 15;         -- OK.

      -- Use constant Max_Decimal_Digits in the definition of a decimal 
      -- type's digits value.
      type Dec_Type_5 is 
        delta 10.0**(-Ada.Decimal.Max_Scale) 
        digits Ada.Decimal.Max_Decimal_Digits;                          -- OK.

      type Dec_Type_6 is 
        delta 10.0**(-Ada.Decimal.Min_Scale) 
        digits Ada.Decimal.Max_Decimal_Digits;                          -- OK.

      type Dec_Type_7 is 
        delta Ada.Decimal.Min_Delta 
        digits Ada.Decimal.Max_Decimal_Digits;                          -- OK.

      type Dec_Type_8 is 
        delta Ada.Decimal.Max_Delta 
        digits Ada.Decimal.Max_Decimal_Digits;                          -- OK.


      -- Ensure that each of the types defined correctly above can have the
      -- Machine_Radix set to either 2 or 10.
      for Dec_Type_1'Machine_Radix use  2;                              -- OK.
      for Dec_Type_2'Machine_Radix use 10;                              -- OK.
      for Dec_Type_3'Machine_Radix use  2;                              -- OK.
      for Dec_Type_4'Machine_Radix use 10;                              -- OK.
      for Dec_Type_5'Machine_Radix use  2;                              -- OK.
      for Dec_Type_6'Machine_Radix use 10;                              -- OK.
      for Dec_Type_7'Machine_Radix use  2;                              -- OK.
      for Dec_Type_8'Machine_Radix use 10;                              -- OK.


      -- Demonstrate that values larger/smaller than the constants
      -- tested above, when used in the declaration of a decimal
      -- fixed point, are illegal.
      type Dec_Type_9 is 
        delta 10.0**(-(Ada.Decimal.Max_Scale+1)) digits 8;           -- ERROR:
                                                       -- Value above maximum.
      type Dec_Type_10 is 
        delta 10.0**(-(Ada.Decimal.Min_Scale-1)) digits 6;           -- ERROR:
                                                       -- Value below minimum.

      type Dec_Type_11 is 
        delta Ada.Decimal.Min_Delta - 0.0001 digits 10;              -- ERROR:
                                                       -- Value below minimum.

      type Dec_Type_12 is 
        delta Ada.Decimal.Max_Delta + 0.0001 digits 15;              -- ERROR:
                                                       -- Value above maximum.
      type Dec_Type_13 is 
        delta 10.0**(-Ada.Decimal.Max_Scale) 
        digits Ada.Decimal.Max_Decimal_Digits + 1;                   -- ERROR:
                                                       -- Value above maximum.
      type Dec_Type_14 is 
        delta 10.0**(-Ada.Decimal.Min_Scale) 
        digits Ada.Decimal.Max_Decimal_Digits + 1;                   -- ERROR:
                                                       -- Value above maximum.
      type Dec_Type_15 is 
        delta Ada.Decimal.Min_Delta 
        digits Ada.Decimal.Max_Decimal_Digits + 1;                   -- ERROR:
                                                       -- Value above maximum.
      type Dec_Type_16 is 
        delta Ada.Decimal.Max_Delta 
        digits Ada.Decimal.Max_Decimal_Digits + 1;                   -- ERROR:
                                                       -- Value above maximum.
   begin
      null;
   end Decimal_Block;


end BXF1001;
