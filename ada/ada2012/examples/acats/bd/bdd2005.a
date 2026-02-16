-- BDD2005.A
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
--        Check that subprograms specified with aspect_specifications
--        for stream attributes meet the requirements for such subprograms.
--
-- TEST DESCRIPTION:
--
--     This test checks the Binding Interpretations of AI05-0007-1 and
--     AI05-0039-1 along with the older requirements of Ada 95.
--
--     We check that each of the following requirements on
--     user-defined subprograms specified for stream attributes are enforced:
--      (A) The specified routine must be match the profile of and be mode
--          conformant with the attributes (13.3(4) and 13.3(6));
--      (B) The specified routine must have the convention Ada (13.3(6));
--      (C) The specified routine must not be abstract (13.13.2(38/3));
--      (D) The specified routine must be statically denoted
--          (13.13.2(38/3), as modified by AI05-0039-1);
--      (E) If the type is an interface type, the specified routine must be
--          a null procedure (13.13.2(38/4)) if the aspect is not class-wide;
--      (F) The subtype of the object parameter or result of the specified
--          routine must be either the first subtype or the base type for
--          scalars and must be the the first subtype for non-scalars
--          (13.13.2(51/3), as modified by AI05-0007-1);
--      (G) The aspect must be specified on a first subtype (by 13.1(8.1/1));
--      (H) A particular stream attribute can only be specified once
--          for a type (13.1(9.1/1));
--      (I) A stream attribute can be specified for a private view
--          (13.1(9.1/1) - Corrigendum change).
--
--     The other rules checked by the companion test (BDD2004) don't
--     make sense for attribute specifications.
--
-- CHANGE HISTORY:
--        29 Jan 07   RLB     Created test (for RR Software).
--        31 Jan 07   RLB     Added more subtests and corrected existing ones.
--        05 Dec 13   RLB     Converted to ACATS format, added interface cases,
--                            made aspect specification version.
--        28 Feb 14   RLB     Changes to issue in ACATS 3.0; commented
--                            out Ada 2012-specific cases.
--        01 Mar 14   RLB     Corrected minor test errors.
--        09 Jul 14   RLB     Commented out Write'Class and similar cases
--                            pending the resolution of AI12-0106-1.
--        20 Nov 14   RLB     Corrected test with the approval of AI12-0121-1,
--                            and replaced the test cases for AI12-0106-1.
--        19 Mar 15   RLB     Eliminated overlong lines.
--
--!
with Ada.Streams;
package BDD2005 is

    type Foobar1 is null record
       with Read => Not_Read;          -- ERROR: (A) Wrong number of
                                       --            parameters.

    procedure Not_Read (A : out Foobar1);

    type Foobar2 is null record
       with Write => Not_Write;        -- ERROR: (A) Wrong mode.

    procedure Not_Write
       (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
        A : out Foobar2);

    type Foobar3 is null record
       with Output => Not_Output;      -- ERROR: (A) Wrong type.

    procedure Not_Output
       (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
        A : in Integer);

    type Foobar4 is null record
       with Input => Not_Input;        -- ERROR: (A) Not function.

    procedure Not_Input
       (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
        A : out Foobar4);

    type My_Int is range 0 .. 100
        with Read  => AI007_Read,      -- OK. (Changed by AI05-0007-1).
             Write => Good_Write1;     -- OK.

    procedure AI007_Read
       (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
        A : out My_Int);

    procedure Good_Write1
       (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
        A : in My_Int'Base);

    type My_Int2 is range 0 .. 100
        with Write => Good_Write2,
             Write => Second_Write;    -- ERROR: (H) Specified twice.

    procedure Good_Write2
       (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
        A : in My_Int2'Base);

    procedure Second_Write
       (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
        A : in My_Int2'Base);

    -- Note: We can't test this because we can't assume any convention
    -- other than Ada is supported. We could use a macro to set such
    -- a convention but it is not worth it. This case is left so that
    -- implementers that support C (about 99% of them) can uncomment this
    -- for local testing.
    --type My_Int2 is range 0 .. 100
    --    with Output => Bad_Output1;  -- ERROR: (B) Wrong convention.
    --
    --procedure Bad_Output1
    --   (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
    --    A : in My_Int'Base)
    --    with Convention => C;

    type My_Int3 is range 0 .. 100
        with Input => Not_Input2;      -- ERROR: (A) Wrong return type.

    function Not_Input2
       (Stream : not null access Ada.Streams.Root_Stream_Type'Class)
           return Integer;

    type My_Tagged_1 is abstract tagged null record
       with Read => Bad_Read3;         -- ERROR: (C) Routine is abstract.

    procedure Bad_Read3
       (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
        A : out My_Tagged_1) is abstract;

    type My_Tagged_2 is abstract tagged null record
       with Write => Bad_Write3;       -- ERROR: (A) Not access parameter.

    procedure Bad_Write3
       (Stream : in out Ada.Streams.Root_Stream_Type'Class;
        A : in My_Tagged_2);

    type My_Tagged_3 is abstract tagged null record
       with Output => Good_Output3;    -- OK.

    procedure Good_Output3
       (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
        A : in My_Tagged_3);

    -- A function of My_Tagged_x would be illegal unless abstract.

    type My_Tagged_4 is abstract tagged null record
       with Write'Class => Good_Write3;-- OK.

    procedure Good_Write3
       (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
        A : in My_Tagged_4'Class);

    type My_Priv_1 (D : Integer) is private
       with Read => Bad_Read4;         -- ERROR: (F) Wrong subtype.
    subtype Const_Priv_1 is My_Priv_1(10);

    procedure Bad_Read4
       (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
        A : out Const_Priv_1);

    type My_Priv_2 (D : Integer) is private
       with Input => Good_Input4;      -- OK (I).
    subtype Const_Priv_2 is My_Priv_2(10)
       with Write => Bad_Write4;       -- ERROR: (G) Not first subtype.

    procedure Bad_Write4 (Stream : in out Ada.Streams.Root_Stream_Type'Class;
                          A : in Const_Priv_2);

    function Good_Input4
       (Stream : not null access Ada.Streams.Root_Stream_Type'Class)
                   return My_Priv_2;


    type My_Big_Int is range 0 .. 10000
       with Read => RPtr.all;         -- ERROR: (D) Not statically denoted.

    type Read_Ptr is access procedure
      (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
       A : out My_Big_Int'Base);

    procedure Good_Read6
       (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
        A : out My_Big_Int'Base);

    RPtr : Read_Ptr := Good_Read6'Access; -- Does not freeze
                                          -- My_Big_Int (AI05-0019-1).

    type My_Big_Int_2 is range 0 .. 10000;

    type My_Big_Int_3 is range 0 .. 10000
       with Output => My_Big_Int_2'Write;-- ERROR: (D) Not statically denoted.

    type Intf1 is limited interface
       with Read'Class => Good_Read7;  -- OK.

    procedure Good_Read7
       (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
        A : out Intf1'Class);

    type Intf2 is limited interface
       with Write'Class => Good_Write7;-- OK.

    procedure Good_Write7
       (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
        A : in Intf2'Class) is null;

    type Intf3 is limited interface
       with Input'Class => Bad_Input7; -- ERROR: (C) is abstract.

    function Bad_Input7
       (Stream : not null access Ada.Streams.Root_Stream_Type'Class)
           return Intf3'Class is abstract;

    type Intf4 is limited interface
       with Output => Inner.Bad_Output7; -- ERROR: (E) not a null procedure.

    package Inner is
       procedure Bad_Output7
          (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
           A : in Intf4);
          -- Can't be primitive.
    end Inner;

private
    type My_Priv_1 (D : Integer) is null record;
    type My_Priv_2 (D : Integer) is null record;
end BDD2005;

