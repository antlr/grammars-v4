-- B732001.A
--
--                             Grant of Unlimited Rights
--
--     The Ada Conformity Assessment Authority (ACAA) holds unlimited
--     rights in the software and documentation contained herein. Unlimited
--     rights are the same as those granted by the U.S. Government for older
--     parts of the Ada Conformity Assessment Test Suite, and are defined
--     in DFAR 252.227-7013(a)(19). By making this public release, the ACAA
--     intends to confer upon all recipients unlimited rights equal to those
--     held by the ACAA. These rights include rights to use, duplicate,
--     release or disclose the released technical data and computer software
--     in whole or in part, in any manner and for any purpose whatsoever, and
--     to have or permit others to do so.
--
--                                    DISCLAIMER
--
--     ALL MATERIALS OR INFORMATION HEREIN RELEASED, MADE AVAILABLE OR
--     DISCLOSED ARE AS IS. THE ACAA MAKES NO EXPRESS OR IMPLIED
--     WARRANTY AS TO ANY MATTER WHATSOEVER, INCLUDING THE CONDITIONS OF THE
--     SOFTWARE, DOCUMENTATION OR OTHER INFORMATION RELEASED, MADE AVAILABLE
--     OR DISCLOSED, OR THE OWNERSHIP, MERCHANTABILITY, OR FITNESS FOR A
--     PARTICULAR PURPOSE OF SAID MATERIAL.
--
--                                     Notice
--
--     The ACAA has created and maintains the Ada Conformity Assessment Test
--     Suite for the purpose of conformity assessments conducted in accordance
--     with the International Standard ISO/IEC 18009 - Ada: Conformity
--     assessment of a language processor. This test suite should not be used
--     to make claims of conformance unless used in accordance with
--     ISO/IEC 18009 and any applicable ACAA procedures.
--*
-- OBJECTIVE:
--     Check that the expression for an aspect Type_Invariant cannot have
--     a non-boolean type.
--
--     Check that aspect Type_Invariant cannot be specified on an
--     abstract type.
--
--     Check that the expression for an aspect Type_Invariant can be of
--     any boolean type.
--
-- CHANGE HISTORY:
--      16 Jan 15   RLB     Created test.
--      23 Jan 15   RLB     Corrected test errors.
--      12 Mar 15   RLB     Fixed line too long.
--
--!
with Ada.Assertions;

package B732001 is

   pragma Assertion_Policy (Check);

   type Bool_Like is new Boolean;

   Maybe : constant Bool_Like := True;

   type Priv01 is private
      with Type_Invariant => Maybe;                                  -- OK.

   type Priv02 is private
      with Type_Invariant => Is_Valid (Priv02);                      -- OK.

   function Is_Valid (Obj : in Priv02) return Bool_Like;

   type Priv03 is private
      with Type_Invariant => Length (Priv03);                        -- ERROR:
      -- Forgot "/= 0".

   function Length (Obj : in Priv03) return Natural;

   type Priv04 is abstract tagged limited private
      with Type_Invariant => Is_Valid (Priv04);                      -- ERROR:
      -- The famous "keyword storm".

   function Is_Valid (Obj : in Priv04) return Boolean;

   type Priv05 is tagged private;

   function Length (Obj : in Priv05) return Natural;

   type Priv06 is private;

   type Priv07 is abstract tagged private;

   function Is_Valid (Obj : in Priv07) return Boolean;


   type Tag_Priv is tagged private;

   function Is_Valid (Obj : in Tag_Priv) return Boolean;


   type Ext1 is new Tag_Priv with private
      with Type_Invariant => Is_Clean (Ext1);                        -- OK.

   function Is_Clean (Obj : in Ext1) return Bool_Like;

   type Ext2 is new Tag_Priv with private
      with Type_Invariant => Length (Ext2);                          -- ERROR:

   function Length (Obj : in Ext2) return Natural;

   type Ext3 is abstract new Tag_Priv with private
      with Type_Invariant => Is_Valid (Ext3);                        -- ERROR:

   function Is_Valid (Obj : in Ext3) return Boolean;

   type Ext4 is new Tag_Priv with private;

   type Ext5 is new Tag_Priv with private;

   function Length (Obj : in Ext5) return Natural;

   type Ext6 is abstract new Tag_Priv with private;

   function Is_Valid (Obj : in Ext6) return Boolean;

private

   -- Completions for declarations above:

   type Priv01 is range 1 .. 10;

   type Priv02 is range 1 .. 10;

   function Is_Valid (Obj : in Priv02) return Bool_Like is
      (Bool_Like(Obj = 4));

   type Priv03 is range 1 .. 10;

   function Length (Obj : in Priv03) return Natural is (Natural(Obj));

   type Priv04 is abstract tagged limited null record;

   function Is_Valid (Obj : in Priv04) return Boolean is (True);

   type Tag_Priv is tagged record
      Len : Integer;
   end record
      with Type_Invariant => Is_Valid (Tag_Priv);                    -- OK.

   function Is_Valid (Obj : in Tag_Priv) return Boolean is (Obj.Len > 0);

   type Ext1 is new Tag_Priv with null record;

   function Is_Clean (Obj : in Ext1) return Bool_Like is
      (Bool_Like(Is_Valid (Tag_Priv(Obj))));

   type Ext2 is new Tag_Priv with null record;

   function Length (Obj : in Ext2) return Natural is (Obj.Len);

   type Ext3 is abstract new Tag_Priv with null record;

   function Is_Valid (Obj : in Ext3) return Boolean is (Obj.Len > 0);


   -- Checks for uses on completions:

   type Priv05 is tagged record
      Len : Natural := 0;
   end record
      with Type_Invariant => Length (Priv05);                        -- ERROR:

   function Length (Obj : in Priv05) return Natural is (Obj.Len);

   type Priv06 is range 1 .. 10
      with Type_Invariant => Maybe;                                  -- OK.

   type Priv07 is abstract tagged null record
      with Type_Invariant => Is_Valid (Priv07);                      -- ERROR:

   function Is_Valid (Obj : in Priv07) return Boolean is (True);

   type Ext4 is new Tag_Priv with null record
      with Type_Invariant => Maybe;                                  -- OK.

   type Ext5 is new Tag_Priv with null record
      with Type_Invariant => Length (Ext5);                          -- ERROR:

   function Length (Obj : in Ext5) return Natural is (Obj.Len);

   type Ext6 is abstract new Tag_Priv with null record
      with Type_Invariant => Is_Valid (Ext6);                        -- ERROR:

   function Is_Valid (Obj : in Ext6) return Boolean is (Obj.Len > 0);

end B732001;

