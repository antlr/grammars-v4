-- B413003.A
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
--     Check that the prefixed view L.R is illegal if the first parameter
--     of R is a parameter with mode in out or out and L does not denote
--     a variable.
--
--     Check that the prefixed view L.R is illegal if the first parameter
--     of R is a parameter with an access-to-variable type and L does not
--     denote a variable.
--
-- CHANGE HISTORY:
--     27 Sep 2007 RLB Created test.
--
--!
procedure B413003 is
   package Pack is
      type T is tagged record
         Data      : Integer := 999;
      end record;

      procedure PIn    (X : in T);
      procedure PInOut (X : in out T);
      procedure POut   (X : out T);
      procedure PAccV  (X : access T);
      procedure PAccC  (X : access constant T);
      procedure PInOutCl (X : in out T'Class);
      procedure PAccVCl  (X : access T'Class);
      function FIn   (X : in T) return Integer;
      function FAccV (X : access T) return Integer;
      function FAccC (X : access constant T) return Integer;
      function FInCl   (X : in T'Class) return Integer;
      function FAccVCl (X : access T'Class) return Integer;

      function F_T   return T;
      function F_TC  return T'Class;
      function F_AT  return access T;
      function F_ATC return access T'Class;

   end Pack;

   package body Pack is
      procedure PIn    (X : in T) is
      begin
         null;
      end PIn;

      procedure PInOut (X : in out T) is
      begin
         null;
      end PInOut;

      procedure POut   (X : out T) is
      begin
         null;
      end POut;

      procedure PAccV  (X : access T) is
      begin
         null;
      end PAccV;

      procedure PAccC  (X : access constant T) is
      begin
         null;
      end PAccC;

      procedure PInOutCl (X : in out T'Class) is
      begin
         null;
      end PInOutCl;

      procedure PAccVCl  (X : access T'Class) is
      begin
         null;
      end PAccVCl;

      function FIn   (X : in T) return Integer is
      begin
         return 0;
      end FIn;

      function FAccV (X : access T) return Integer is
      begin
         return 0;
      end FAccV;

      function FAccC (X : access constant T) return Integer is
      begin
         return 0;
      end FAccC;

      function FInCl   (X : in T'Class) return Integer is
      begin
         return 0;
      end FInCl;

      function FAccVCl (X : access T'Class) return Integer is
      begin
         return 0;
      end FAccVCl;


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
   OCA : aliased constant Pack.T := (Data => 0);
   OVA : aliased Pack.T;
   OCAC: constant Pack.T'Class := Pack.F_TC;
   OVAC: Pack.T'Class := Pack.F_TC;
   type Arr is array (1..2) of Pack.T;
   OCAA : constant Arr := (others => (Data => 0));
   OVAA : Arr;
   OCAT : access constant Pack.T := new Pack.T'(Data => 0);
   OVAT : access Pack.T := new Pack.T'(Data => 0);
   RenCA : Pack.T renames OCA;
   RenVA : Pack.T renames OVA;

begin
   OCA.PIn;                -- OK.
   OVA.PIn;                -- OK.
   OCA.PInOut;             -- ERROR:
   OVA.PInOut;             -- OK.
   OCAC.POut;              -- ERROR:
   OVAC.POut;              -- OK.
   OCAT.PAccV;             -- ERROR:
   OVAT.PAccV;             -- OK.
   OCAT.PAccC;             -- OK.
   OVAT.PAccC;             -- OK.
   OCAA(1).PInOutCl;       -- ERROR:
   OVAA(2).PInOutCl;       -- OK.
   RenCA.PAccVCl;          -- ERROR:
   RenVA.PAccVCl;          -- OK.
   R := OCA.FIn;           -- OK.
   R := OVA.FIn;           -- OK.
   R := OCA.FAccV;         -- ERROR:
   R := OVA.FAccV;         -- OK.
   R := OCA.FAccC;         -- OK.
   R := OVA.FAccC;         -- OK.
   R := OCA.FInCl;         -- OK.
   R := OVA.FInCl;         -- OK.
   R := OCA.FAccVCl;       -- ERROR:
   R := OVA.FAccVCl;       -- OK.
   Pack.F_T.PInOut;        -- ERROR:
   Pack.F_TC.POut;         -- ERROR:
   Pack.F_AT.PAccV;        -- OK.
   Pack.F_ATC.PInOutCl;    -- OK.
   OCAT.all.PAccV;         -- ERROR:
   OVAT.all.PAccV;         -- OK.
   declare
      procedure Proc (P : in Pack.T) is
      begin
         P.PIn;            -- OK.
         P.PInOut;         -- ERROR:
         P.POut;           -- ERROR:
         P.PAccV;          -- ERROR:
         P.PAccC;          -- OK.
         P.PInOutCl;       -- ERROR:
         P.PAccVCl;        -- ERROR:
      end Proc;
   begin
      Proc (OCA);
   end;
end B413003;
