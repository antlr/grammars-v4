-- B3A0004.A
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
--      For an array object X used as the prefix for the attributes
--      X'Access or X'Unchecked_Access, where the expected type for
--      X'Access or X'Unchecked_Access is the general access type A:
--
--        Check that the nominal subtype of an aliased view of X must
--        statically match A's designated array subtype.
--
-- TEST DESCRIPTION:
--      This test defines two general access types (one constant, one
--      variable) to the predefined array type String.  It defines one
--      aliased variable of each form combining constrained or unconstrained,
--      variable or constant.  It then checks that the use of the attributes
--      'Access and 'Unchecked_Access conform to the static subtype matching
--      requirements.
--
--      A similar check is performed on a user defined array type.
--
--
-- CHANGE HISTORY:
--      19 MAR 96   SAIC   Initial version
--      04 NOV 96   SAIC   Revised for 2.1 release
--
--!

------------------------------------------------------------------- B3A0004

with Report;

procedure B3A0004 is

    -- access-to-constant array type and object
  type Access_Constant_String is access constant String;

  Access_Constant : Access_Constant_String;

    -- aliased constant constrained array object
    -- nominal subtype is String (constrained 1..4)
  Constrained_Constant   : aliased constant String(1..4) := "ACVC";

    -- aliased constant unconstrained array object
    -- nominal subtype is String (unconstrained)
  Unconstrained_Constant : aliased constant String := "SAIC";

   -- access-to-variable array type and object
  type Access_All_String is access all String;

  Access_Variable : Access_All_String;

    -- aliased variable constrained array object
    -- nominal subtype is String (constrained 1..4)
  Constrained_String   : aliased String(1..4) := "AJPO";

    -- aliased variable unconstrained array object
    -- nominal subtype is String (unconstrained)
  Unconstrained_String : aliased String := "NIST";

  type User_Type is array(Character range <>) of Float;

  type Access_Constant_User is access constant User_Type;

  type Access_Variable_User is access all User_Type;

  Access_C_User : Access_Constant_User;

  Access_V_User : Access_Variable_User;

  Constrained_Const_User   : aliased constant User_Type('a'..'b')
                           := (0.0,0.0);

  Unconstrained_Const_User : aliased constant User_Type := (0.0,0.0);

  Constrained_Var_User     : aliased User_Type('1'..'2') := (0.0,0.0);

  Unconstrained_Var_User   : aliased User_Type := (0.0,0.0);

  subtype FourString is String(1..4);
  type Access_Constant_String_4 is access constant FourString;

  Access_Constant_4      : Access_Constant_String_4;
  Constrained_Constant_4 : aliased constant FourString := "ACVC";

begin  -- Main test procedure.

  Access_Constant := Unconstrained_Constant'Access;                  -- OK

  Access_Constant := Constrained_Constant'Access;                    -- ERROR:
                    -- object subtype must statically match designated subtype

  Access_Variable := Unconstrained_String'Access;                    -- OK

  Access_Variable := Constrained_String'Access;                      -- ERROR:
                    -- object subtype must statically match designated subtype

  Access_Constant := Unconstrained_Constant'Unchecked_Access;        -- OK

  Access_Constant := Constrained_Constant'Unchecked_Access;          -- ERROR:
                    -- object subtype must statically match designated subtype

  Access_Variable := Unconstrained_String'Unchecked_Access;          -- OK

  Access_Variable := Constrained_String'Unchecked_Access;            -- ERROR:
                    -- object subtype must statically match designated subtype

  Access_C_User :=  Unconstrained_Const_User'Access;                 -- OK

  Access_C_User :=  Constrained_Const_User'Access;                   -- ERROR:
                    -- object subtype must statically match designated subtype

  Access_V_User :=  Unconstrained_Var_User'Access;                   -- OK

  Access_V_User :=  Constrained_Var_User'Access;                     -- ERROR:
                    -- object subtype must statically match designated subtype

  Access_Constant_4 := Constrained_Constant_4'Access;                -- OK

  Access_Constant_4 := Constrained_Constant'Access;                  -- OK

  Access_Constant_4 := Unconstrained_Constant'Access;                -- ERROR:
                    -- object subtype must statically match designated subtype

end B3A0004;
