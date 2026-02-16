-- B413002.A
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
--  OBJECTIVE:
--
--      Check that the prefixed view L.R is illegal if the first parameter
--      of R is an access parameter and L is not an aliased view of an object.
--
-- CHANGE HISTORY:
--     27 Sep 2007 RLB Created test.
--
--!
procedure B413002 is
   package Pack is
      type T is tagged record
         Data      : Integer := 999;
      end record;

      procedure PC  (X : access constant T);
      procedure PV  (X : access T);
      procedure PCl (X : access T'Class);
      function FC  (X : access constant T) return Integer;
      function FV  (X : access T) return Integer;
      function FCl (X : access T'Class) return Integer;

      function F_T   return T;
      function F_TC  return T'Class;
      function F_AT  return access T;
      function F_ATC return access T'Class;

   end Pack;

   package body Pack is
      procedure PC  (X : access constant T) is
      begin
         null;
      end PC;

      procedure PV  (X : access T) is
      begin
         null;
      end PV;

      procedure PCl (X : access T'Class) is
      begin
         null;
      end PCl;

      function FC  (X : access constant T) return Integer is
      begin
         return 0;
      end FC;

      function FV  (X : access T) return Integer is
      begin
         return 0;
      end FV;

      function FCl (X : access T'Class) return Integer is
      begin
         return 0;
      end FCl;

      function F_T   return T is
      begin
         return T'(Data => 0);
      end F_T;

      function F_TC  return T'Class is
      begin
         return T'(Data => 0);
      end F_TC;

      function F_AT  return access T is
      begin
         return new T'(Data => 0);
      end F_AT;

      function F_ATC return access T'Class is
      begin
         return new T'(Data => 0);
      end F_ATC;

   end Pack;

   R : Integer;
   O_A : aliased Pack.T;
   ONA : Pack.T;
   O_AC: aliased Pack.T'Class := Pack.F_TC;
   ONAC: Pack.T'Class := Pack.F_TC;
   type Arr is array (1..2) of Pack.T;
   OAA : Arr;
   O_AT : access Pack.T := new Pack.T'(Data => 0);
   Ren_A : Pack.T renames O_A;
   RenNA : Pack.T renames ONA;
begin
   O_A.PC;                  -- OK.
   ONA.PV;                  -- ERROR:
   O_AC.PCl;                -- OK.
   R := ONAC.FC;            -- ERROR:
   R := OAA(1).FV;          -- ERROR:
   R := O_AT.all.FCl;       -- OK.
   O_AT.PC;                 -- OK.
   Pack.F_T.PV;             -- ERROR:
   Pack.F_TC.PCl;           -- ERROR:
   R := Pack.F_AT.FC;       -- OK.
   R := Pack.F_ATC.FV;      -- OK.
   R := Ren_A.FCl;          -- OK.
   RenNA.PC;                -- ERROR:
   declare
      procedure Proc (P : in Pack.T) is
      begin
         R := P.FC;         -- OK. (Parameters of tagged types are
         P.PC;              -- OK.  implicitly aliased).
      end Proc;
   begin
      Proc (O_A);
   end;
end B413002;
