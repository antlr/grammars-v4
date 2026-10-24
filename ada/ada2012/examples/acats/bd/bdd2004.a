-- BDD2004.A
--
--                                     Grant of Unlimited Rights
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
--                                                DISCLAIMER
--
--     ALL MATERIALS OR INFORMATION HEREIN RELEASED, MADE AVAILABLE OR
--     DISCLOSED ARE AS IS. THE ACAA MAKES NO EXPRESS OR IMPLIED
--     WARRANTY AS TO ANY MATTER WHATSOEVER, INCLUDING THE CONDITIONS OF THE
--     SOFTWARE, DOCUMENTATION OR OTHER INFORMATION RELEASED, MADE AVAILABLE
--     OR DISCLOSED, OR THE OWNERSHIP, MERCHANTABILITY, OR FITNESS FOR A
--     PARTICULAR PURPOSE OF SAID MATERIAL.
--
--                                                 Notice
--
--     The ACAA has created and maintains the Ada Conformity Assessment Test
--     Suite for the purpose of conformity assessments conducted in accordance
--     with the International Standard ISO/IEC 18009 - Ada: Conformity
--     assessment of a language processor. This test suite should not be used
--     to make claims of conformance unless used in accordance with
--     ISO/IEC 18009 and any applicable ACAA procedures.
--*
-- OBJECTIVE:
--        Check that subprograms specified with attribute_definition_clauses
--        for stream attributes meet the requirements for such subprograms.
--
-- TEST DESCRIPTION:
--
--        This test checks the Binding Interpretations of AI05-0007-1 and
--        AI05-0039-1 along with the older requirements of Ada 95.
--
--        We check that each of the following requirements on
--        user-defined subprograms specified for stream attributes are
---       enforced:
--        (A) The specified routine must be match the profile of and be mode
--            conformant with the attributes (13.3(4) and 13.3(6));
--        (B) The specified routine must have the convention Ada (13.3(6));
--        (C) The specified routine must not be abstract (13.13.2(38/3));
--        (D) The specified routine must be statically denoted
--            (13.13.2(38/3), as modified by AI05-0039-1);
--        (E) If the type is an interface type, the specified routine must be
--            a null procedure (13.13.2(38/3));
--        (F) The subtype of the object parameter or result of the specified
--            routine must be either the first subtype or the base type for
--            scalars and must be the the first subtype for non-scalars
--            (13.13.2(51/3), as modified by AI05-0007-1);
--        (G) The type name for the attribute in the
--            attribute_definition_clause must be the first subtype
--            (by 13.1(8.1/1));
--        (H) The type cannot be frozen before the attribute_definition_clause
--            is given (13.1(9.1/1));
--        (I) A particular stream attribute can only be specified once
--            for a type (13.1(9.1/1));
--        (J) A stream attribute can be specified for a private view
--            (13.1(9.1/1) - Corrigendum change);
--        (K) The attribute_definition_clause must occur in the same
--            declarative part as the type (13.1(5/1)).
--
-- CHANGE HISTORY:
--        29 Jan 07   RLB     Created test (for RR Software).
--        31 Jan 07   RLB     Added more subtests and corrected existing ones.
--        05 Dec 13   RLB     Converted to ACATS format, added interface cases.
--        27 Dec 13   RLB     Changes to issue in ACATS 3.0; commented
--                            out Ada 2012-specific cases.
--        05 Feb 14   RLB     Corrected test errors.
--        28 Feb 14   RLB     Replaced Ada 2012-specific cases.
--        20 Nov 14   RLB     Repaired for AI12-0121-1.
--        19 Mar 15   RLB     Eliminated overlong lines.
--
--!
with Ada.Streams;
package BDD2004 is

    type Foobar is null record;

    procedure Not_Read (A : out Foobar);

    for Foobar'Read use Not_Read;     -- ERROR: (A) Wrong number of parameters.

    procedure Not_Write
       (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
        A : out Foobar);

    for Foobar'Write use Not_Write;   -- ERROR: (A) Wrong mode.

    procedure Not_Output
       (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
        A : in Integer);

    for Foobar'Output use Not_Output; -- ERROR: (A) Wrong type.

    procedure Not_Input
       (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
        A : out Foobar);

    for Foobar'Input use Not_Input;   -- ERROR: (A) Not function.

    type My_Int is range 0 .. 100;

    procedure AI007_Read
       (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
        A : out My_Int);

    for My_Int'Read use AI007_Read;   -- OK. (Required by AI05-0007-1).

    procedure Good_Write1
       (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
        A : in My_Int'Base);

    for My_Int'Write use Good_Write1; -- OK.

    procedure Second_Write
       (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
        A : in My_Int'Base);

    for My_Int'Write use Second_Write;-- ERROR: (I) Specified twice.

    -- Note: We can't test this because we can't assume any convention
    -- other than Ada is supported. We could use a macro to set such
    -- a convention but it is not worth it. This case is left so that
    -- implementers that support C (about 99% of them) can uncomment this
    -- for local testing.
    --procedure Bad_Output1
    --   (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
    --    A : in My_Int'Base);
    --pragma Convention (C, Bad_Output1);
    --
    --for My_Int'Output use Bad_Output1; -- ERROR: (B) Wrong convention.

    function Not_Input2
       (Stream : not null access Ada.Streams.Root_Stream_Type'Class)
          return Integer;

    for My_Int'Input use Not_Input2;  -- ERROR: (A) Wrong return type.

    type My_Float is digits 6;

    Obj : My_Float := 10.0; -- Freeze My_Float.

    procedure Bad_Write2
       (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
        A : in My_Float'Base);

    for My_Float'Write use Bad_Write2;-- ERROR: (H) Type is frozen.

    type My_Tagged is abstract tagged null record;

    procedure Bad_Read3
       (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
        A : out My_Tagged) is abstract;

    for My_Tagged'Read use Bad_Read3; -- ERROR: (C) Routine is abstract.

    procedure Bad_Write3 (Stream : in out Ada.Streams.Root_Stream_Type'Class;
                          A : in My_Tagged);

    for My_Tagged'Write use Bad_Write3; -- ERROR: (A) Not access parameter.

    procedure Good_Output3
       (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
        A : in My_Tagged);

    for My_Tagged'Output use Good_Output3; -- OK.

    -- A function of My_Tagged would be illegal unless abstract.

    procedure Good_Write3
       (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
        A : in My_Tagged'Class);

    for My_Tagged'Class'Write use Good_Write3; -- OK.


    type My_Priv (D : Integer) is private;
    subtype Const_Priv is My_Priv(10);

    procedure Bad_Read4
       (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
        A : out Const_Priv);

    for My_Priv'Read use Bad_Read4;   -- ERROR: (F) Wrong subtype.

    procedure Bad_Write4 (Stream : in out Ada.Streams.Root_Stream_Type'Class;
                          A : in Const_Priv);

    for Const_Priv'Write use Bad_Write4; -- ERROR: (G) Not first subtype.

    procedure Good_Output4
       (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
        A : in My_Priv);

    for Const_Priv'Output use Good_Output4; -- ERROR: (G) Not first subtype.

    function Good_Input4
       (Stream : not null access Ada.Streams.Root_Stream_Type'Class)
          return My_Priv;

    for My_Priv'Input use Good_Input4; -- OK. (J)


    type My_Bad is range 0 .. 100;

    package Nested is
          type My_Awful is range 0 .. 200;

          procedure Good_Output5
             (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
              A : in My_Bad'Base);

          for My_Bad'Write use Good_Output5; -- ERROR: (K) Not in same
                                             --            declarative part.
    end Nested;

    for My_Bad'Output use Nested.Good_Output5; -- OK.

    type My_Big_Int is range 0 .. 10000;

    type Write_Ptr is access procedure
          (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
           A : in My_Big_Int'Base);

    procedure Good_Write6
       (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
        A : in My_Big_Int'Base);

    WPtr : Write_Ptr := Good_Write6'Access; -- Does not freeze
                                            -- My_Big_Int (AI05-0019-1).

    for My_Big_Int'Read use WPtr.all; -- ERROR: (A) and (D) Wrong mode,
                                      --        not statically denoted.

    for My_Big_Int'Write use WPtr.all;-- ERROR: (D) Not statically denoted.

    for My_Big_Int'Input use WPtr.all;-- ERROR: (A) and (D) Not function,
                                      --        not statically denoted.

    for My_Big_Int'Output use My_Big_Int'Write; -- ERROR: (D) Not statically
                                                --            denoted.

    type Intf is limited interface;

    procedure Good_Read7
       (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
        A : out Intf'Class);

    for Intf'Class'Read use Good_Read7; -- OK.

    procedure Good_Write7
       (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
        A : in Intf'Class) is null;

    for Intf'Class'Write use Good_Write7; -- OK.

    function Bad_Input7
       (Stream : not null access Ada.Streams.Root_Stream_Type'Class)
           return Intf'Class is abstract;

    for Intf'Class'Input use Bad_Input7; -- ERROR: (C) is abstract.

    package Inner is
       procedure Bad_Output7
          (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
           A : in Intf);
          -- Can't be primitive.
    end Inner;

    for Intf'Output use Inner.Bad_Output7; -- ERROR: (E) not a null procedure.

private
    type My_Priv (D : Integer) is null record;
end BDD2004;

