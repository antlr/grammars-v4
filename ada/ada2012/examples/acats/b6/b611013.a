-- B611013.A
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
--
--*
--
-- OBJECTIVE:
--     For a discrete X, check that the nominal subtype of X'Old is that of X.
--
-- TEST DESCRIPTION:
--     We check that the case completeness checks for a case expression
--     reflect the correct nominal subtype. Case completeness checks are the
--     only legality rules that depend on the nominal subtype of an object.
--
--     We test pairs of case expressions, one with Old and one without;
--     the results should be the same for both.
--
-- CHANGE HISTORY:
--     26 Feb 2016   RLB   Created test.
--     28 Mar 2016   RLB   Added error location codes.
--
--!
package B611013 is

    type Small is range 0 .. 99;

    subtype Tiny is Small range 0 .. 9;

    subtype Prime is Small range 2 .. 7;

    subtype Square is Small range 1 .. 4;

    type Arr is array (Tiny) of Square;

    function Double (N : in Tiny) return Small is (N * 2);

    Glob : Arr;

    -- Exactly right:

    procedure P01 (N : in out Prime)
       with Post =>
          (case N is
             when 2 .. 3 => True,
             when 4 .. 7 => False);                          -- OK. {2:11;1}

    procedure P02 (N : in out Prime)
       with Post =>
          (case N'Old is
             when 2 .. 3 => True,
             when 4 .. 7 => False);                          -- OK. {2:11;1}

    -- 5 .. 7 are extra values:

    procedure P03 (N : in out Prime)
       with Post =>
          (case Glob(N) is
             when 1 .. 3 => True,
             when 4 .. 7 => False);                          -- ERROR: {2:11;1}

    procedure P04 (N : in out Prime)
       with Post =>
          (case Glob(N)'Old is
             when 1 .. 3 => True,
             when 4 .. 7 => False);                          -- ERROR: {2:11;1}

    -- Missing 0 .. 1, 15.. 99:

    procedure P05 (N : in out Prime)
       with Post =>
          (case Double(N) is
             when 2 | 4 | 6 | 8 => True,
             when 3 | 5 | 7 | 9..14 => False);               -- ERROR: {2:11;1}

    procedure P06 (N : in out Prime)
       with Post =>
          (case Double(N)'Old is
             when 2 | 4 | 6 | 8 => True,
             when 3 | 5 | 7 | 9..14 => False);               -- ERROR: {2:11;1}

    -- 0, 1 are outside of the range of the nominal subtype:

    procedure P07 (N : in out Prime)
       with Post =>
          (case Prime'(Glob(Double(N))+1) is
             when 0 .. 7 => True,
             when others => False);                          -- ERROR: {2:11;1}

    procedure P08 (N : in out Prime)
       with Post =>
          (case Prime'(Glob(Double(N))+1)'Old is
             when 0 .. 7 => True,
             when others => False);                          -- ERROR: {2:11;1}

    type Root is tagged record
       T : Tiny;
    end record;

    -- Missing 0, 9:

    procedure P41 (Obj : access Root)
       with Post'Class=>
          (case Obj.T is
             when 1 .. 4 => True,
             when 5 .. 8 => False);                          -- ERROR: {2:11;1}

    procedure P42 (Obj : access Root)
       with Post'Class=>
          (case Obj.T'Old is
             when 1 .. 4 => True,
             when 5 .. 8 => False);                          -- ERROR: {2:11;1}

    -- 0, 5 .. 9 are extra:

    procedure P43 (Obj : access Root)
       with Post'Class=>
          (case Glob(Obj.T) is
             when 0 .. 4 => True,
             when 5 .. 9 => False);                          -- ERROR: {2:11;1}

    procedure P44 (Obj : access Root)
       with Post'Class=>
          (case Glob(Obj.T)'Old is
             when 0 .. 4 => True,
             when 5 .. 9 => False);                          -- ERROR: {2:11;1}

    -- Missing 19 .. 99:

    procedure P45 (Obj : access Root)
       with Post'Class=>
          (case Double(Obj.T) is
             when 0 .. 10 => True,
             when 11 ..18 => False);                         -- ERROR: {2:11;1}

    procedure P46 (Obj : access Root)
       with Post'Class=>
          (case Double(Obj.T)'Old is
             when 0 .. 10 => True,
             when 11 ..18 => False);                         -- ERROR: {2:11;1}

    -- Exactly right:

    procedure P47 (Obj : access Root)
       with Post'Class=>
          (case Glob(Double(Obj.T)) is
             when 1 .. 2 => True,
             when 3 .. 4 => False);                          -- OK. {2:11;1}

    procedure P48 (Obj : access Root)
       with Post'Class=>
          (case Glob(Double(Obj.T))'Old is
             when 1 .. 2 => True,
             when 3 .. 4 => False);                          -- OK. {2:11;1}

end B611013;


