-- B413001.A
--
--                             Grant of Unlimited Rights
--
--     AdaCore holds unlimited rights in the software and documentation
--     contained herein. Unlimited rights are the same as those granted
--     by the U.S. Government for older parts of the Ada Conformity
--     Assessment Test Suite, and are defined in DFAR 252.227-7013(a)(19).
--     By making this public release, AdaCore intends to confer upon all
--     recipients unlimited rights equal to those held by the Ada Conformity
--     Assessment Authority. These rights include rights to use, duplicate,
--     release or disclose the released technical data and computer software
--     in whole or in part, in any manner and for any purpose whatsoever,
--     and to have or permit others to do so.
--
--                                    DISCLAIMER
--
--     ALL MATERIALS OR INFORMATION HEREIN RELEASED, MADE AVAILABLE OR
--     DISCLOSED ARE AS IS. ADACORE MAKES NO EXPRESS OR IMPLIED WARRANTY AS
--     TO ANY MATTER WHATSOEVER, INCLUDING THE CONDITIONS OF THE SOFTWARE,
--     DOCUMENTATION OR OTHER INFORMATION RELEASED, MADE AVAILABLE OR
--     DISCLOSED, OR THE OWNERSHIP, MERCHANTABILITY, OR FITNESS FOR A
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
--
--     This test is based on one submitted by AdaCore; AdaCore retains
--     the copyright on the test.
--
--  OBJECTIVE:
--
--     If L represents an object of a tagged type, check that the reference
--     L.R is not intepreted as a prefixed view if the designator R represents
--     a component of the type T visible at the point of the reference.
--
--  CHANGE HISTORY:
--     16 JUL 2004  JM   Initial version.
--     13 AUG 2004  PHL  The two subprograms named Data were not primitive
--                       operations of type Tp, because they are not declared
--                       in a package specification. Therefore, they are not
--                       candidates for a prefixed view.
--     15 JAN 2005  RLB  Addition of overloading test.
--     26 SEP 2007  RLB  Added parameter test, hidden component test,
--                       converted to ACATS test.

--!
procedure B413001 is
   package Pack is
      type TP is tagged record
         Data      : Integer := 999;
         More_Data : Integer := 111;
         Mega_Data : Integer := 333;
      end record;

      procedure Data (X : in out TP);
      function Data  (X : TP) return Integer;
      function More_Data (X : TP) return Float;
      function Mega_Data (X : TP; P : Integer) return Integer;

      type Priv is tagged private;
      procedure Data (X : in out Priv);
      function Data  (X : Priv) return Float;

   private
      type Priv is tagged record
         Data      : Integer := 999;
      end record;
   end Pack;

   package body Pack is
      procedure Data (X : in out TP) is
      begin
         null;
      end Data;

      function Data  (X : TP) return Integer is
      begin
         return 0;
      end Data;

      function More_Data (X : TP) return Float is
      begin
         return 0.0;
      end More_Data;

      function Mega_Data (X : TP; P : Integer) return Integer is
      begin
         return P;
      end Mega_Data;

      procedure Data (X : in out Priv) is
      begin
         null;
      end Data;

      function Data (X : Priv) return Float is
      begin
         return 0.0;
      end Data;

   end Pack;

   R : Integer;
   O : Pack.TP;
   F : Float;
   P : Pack.Priv;
begin
   O.Data := 10;        -- OK.
   R      := O.Data;    -- OK.
   O.Data;              -- ERROR: Invalid call.

   --  Check that components are not overloaded.

   F := O.More_Data;    -- ERROR: Integer component directly visible.
   R := O.More_Data;    -- OK.

   --  Check that using parameters does not make the prefixed view of a
   --  function visible.
   R := O.Mega_Data;    -- OK.
   R := O.Mega_Data(1); -- ERROR: Parameter on a component.

   --  Check that a hidden component does not prevent using a prefixed view
   --  of a routine.
   P.Data;              -- OK.
   F := P.Data;         -- OK.
   R := P.Data;         -- ERROR: Component not visible/type mismatch.

end B413001;
