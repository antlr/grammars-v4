-- BDE0009.A
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
--    The ACAA has created and maintains the Ada Conformity Assessment Test
--    Suite for the purpose of conformity assessments conducted in accordance
--    with the International Standard ISO/IEC 18009 - Ada: Conformity
--    assessment of a language processor. This test suite should not be used
--    to make claims of conformance unless used in accordance with
--    ISO/IEC 18009 and any applicable ACAA procedures.
--
--*
--
-- OBJECTIVE:
--    Check that implicit calls cause freezing.  (Defect Report 8652/0046, as
--    reflected in Technical Corrigendum 1).
--
--    The subprograms that can be called implicitly are: Initialize, Adjust and
--    Finalize, from Ada.Finalization; Read and Write, from Ada.Streams; and
--    Allocate, Deallocate and Storage_Size, from System.Storage_Pools.  In
--    addition, subprograms that are named in an attribute definition clause
--    (for a stream attribute) may be called implicitly.
--
--    Of these subprograms, the ones from Ada.Streams and System.Storage_Pools
--    are irrelevant for the purposes of this test because they are called
--    through a dispatching call, and even though the dispatching call could
--    dispatch to some user-defined subprogram, this doesn't cause that user-
--    defined subprogram to be frozen.
--
--    The stream attributes Write and Output are irrelevant because they are
--    procedures, and procedures cannot be called in a declarative_part.
--    However, it is possible to call the stream attribute Read, even though it
--    is a procedure, since it can be implicitly called from the default
--    implementation stream attribute Input (which is a function). It is also
--    possible to inherit a user-defined Input attribute.
--
--    There isn't any situation where Finalize would be called before the
--    beginning of a statement part or body, so Finalize is irrelevant.
--
--    The only language-defined things that can be done with an unfrozen
--    Initialize or Adjust routine is give a confirming convention
--    pragma (3.9.2(10/1) requires that it be confirming), an import or
--    export pragma with a confirming convention, or an address clause.
--    These cases can only occur after the freezing of the type, so the
--    implicit call must raise Program_Error. Moreover, it is unlikly that
--    Program_Error would not be raised properly even if the implementation
--    incorrectly handled these cases. And users hardly can be depending
--    on Program_Error being raised rather than a compile-time error. So
--    we do not test Adjust or Initialize calls. [Note: The test cases are
--    left commented out below for the benefit of implementors who want to
--    test this area of their compiler.]
--
--    So, this test checks a number of ways that a user-defined Input
--    attribute, and an user-defined Read attribute may be called implicitly.
--
--
-- CHANGE HISTORY:
--    12 FEB 2001   PHL   Initial version.
--    14 MAR 2003   RLB   Readied for issuing.
--    19 MAY 2003   RLB   Corrected case P10 and added new case P12 to correct
--                        for what actually is called.
--    05 JUN 2003   RLB   Added three cases and corrected three others to
--                        include the effect of 7.6(17.1/1).
--    14 JUL 2003   RLB   Removed all finalization cases, added commentary.
--
--!
with Ada.Finalization;
with Ada.Streams;
use Ada.Finalization;
package BDE0009 is

    --package P1 is
    --
    --	type Ctrl is new Controlled with
    --	    record
    --		C : String (1 .. 3);
    --	    end record;
    --
    --	procedure Initialize (C : in out Ctrl);
    --	procedure Adjust (C : in out Ctrl);
    --	procedure Finalize (C : in out Ctrl);
    --
    --	X : Ctrl;
    --
    --	pragma Convention (Ada, Initialize); -- ERROR: Initialize is frozen
    --	pragma Convention (Ada, Adjust); -- OK
    --	pragma Convention (Ada, Finalize); -- OK
    --end P1;
    --
    --
    --package P2 is
    --
    --	type Ctrl is new Controlled with
    --	    record
    --		C : String (1 .. 3);
    --	    end record;
    --
    --	type Actrl is access Ctrl;
    --
    --	procedure Initialize (C : in out Ctrl);
    --	procedure Adjust (C : in out Ctrl);
    --	procedure Finalize (C : in out Ctrl);
    --
    --	X : Actrl := new Ctrl'((Ada.Finalization.Controlled with "abc"));
    --            -- Does not call Adjust by 7.6(17.1/1)
    --
    --	pragma Convention (Ada, Initialize); -- OK
    --	pragma Convention (Ada, Adjust); -- OK
    --	pragma Convention (Ada, Finalize); -- OK
    --end P2;
    --
    --
    --package P3 is
    --
    --	type Ctrl is new Controlled with
    --	    record
    --		C : String (1 .. 3);
    --	    end record;
    --
    --	type Actrl is access Ctrl;
    --
    --	procedure Initialize (C : in out Ctrl);
    --	procedure Adjust (C : in out Ctrl);
    --	procedure Finalize (C : in out Ctrl);
    --
    --	X : Actrl := new Ctrl'((Ada.Finalization.Controlled with "abc"));
    --	Y : Actrl := new Ctrl'(X.all); -- Calls Adjust
    --
    --	pragma Convention (Ada, Initialize); -- OK
    --	pragma Convention (Ada, Adjust); -- ERROR: Adjust is frozen
    --	pragma Convention (Ada, Finalize); -- OK
    --end P3;
    --
    --
    --package P4 is
    --
    --	type Ctrl is new Controlled with
    --	    record
    --		C : String (1 .. 3);
    --	    end record;
    --
    --	procedure Initialize (C : in out Ctrl);
    --	procedure Adjust (C : in out Ctrl);
    --	procedure Finalize (C : in out Ctrl);
    --
    --	pragma Convention (Ada, Initialize); -- OK
    --	pragma Convention (Ada, Adjust); -- OK
    --	pragma Convention (Ada, Finalize); -- OK
    --
    --	X : Ctrl;
    --end P4;
    --
    --
    --package P5 is
    --
    --	type Ctrl is new Controlled with
    --	    record
    --		C : String (1 .. 3);
    --	    end record;
    --
    --	procedure Initialize (C : in out Ctrl);
    --	procedure Adjust (C : in out Ctrl);
    --	procedure Finalize (C : in out Ctrl);
    --
    --	type Ctrlcomp is
    --	    record
    --		C1 : Ctrl;
    --		C2 : Character;
    --	    end record;
    --	type Actrlcomp is access Ctrlcomp;
    --
    --	X : Actrlcomp := new Ctrlcomp;
    --
    --	pragma Convention (Ada, Initialize); -- ERROR: Initialize is frozen
    --	pragma Convention (Ada, Adjust); -- OK
    --	pragma Convention (Ada, Finalize); -- OK
    --end P5;
    --
    --
    --package P6 is
    --
    --	type Ctrl is new Controlled with
    --	    record
    --		C : String (1 .. 3);
    --	    end record;
    --
    --	procedure Initialize (C : in out Ctrl);
    --	procedure Adjust (C : in out Ctrl);
    --	procedure Finalize (C : in out Ctrl);
    --
    --	type Ctrlcomp is
    --	    record
    --		C1 : Ctrl;
    --		C2 : Character;
    --	    end record;
    --
    --	X : Ctrlcomp := ((Controlled with "bcd"), 'e');
    --            -- Does not call Adjust by 7.6(17.1/1)
    --
    --	pragma Convention (Ada, Initialize); -- OK
    --	pragma Convention (Ada, Adjust); -- OK
    --	pragma Convention (Ada, Finalize); -- OK
    --end P6;
    --
    --
    --package P7 is
    --
    --	type Ctrl is new Controlled with
    --	    record
    --		C : String (1 .. 3);
    --	    end record;
    --
    --	procedure Initialize (C : in out Ctrl);
    --	procedure Adjust (C : in out Ctrl);
    --	procedure Finalize (C : in out Ctrl);
    --
    --	type Ctrlcomp is
    --	    record
    --		C1 : Ctrl;
    --		C2 : Character;
    --	    end record;
    --
    --	X : Ctrlcomp := ((Controlled with "bcd"), 'e');
    --            -- Does not call Adjust by 7.6(17.1/1)
    --    Y : Ctrlcomp := X; -- Does call Adjust
    --
    --	pragma Convention (Ada, Initialize); -- OK
    --	pragma Convention (Ada, Adjust); -- ERROR: Adjust is frozen
    --	pragma Convention (Ada, Finalize); -- OK
    --end P7;
    --
    --
    --package P8 is
    --
    --	type Ctrl is new Controlled with
    --	    record
    --		C : String (1 .. 3);
    --	    end record;
    --
    --	procedure Initialize (C : in out Ctrl);
    --	procedure Adjust (C : in out Ctrl);
    --	procedure Finalize (C : in out Ctrl);
    --
    --	type Ctrlcomp is
    --	    record
    --		C1 : Ctrl;
    --		C2 : Character;
    --	    end record;
    --
    --	pragma Convention (Ada, Initialize); -- OK
    --	pragma Convention (Ada, Adjust); -- OK
    --	pragma Convention (Ada, Finalize); -- OK
    --
    --	X : Ctrlcomp;
    --end P8;
    --
    --
    --package P9 is
    --
    --	type Ctrl is new Controlled with
    --	    record
    --		C : String (1 .. 3);
    --	    end record;
    --
    --	procedure Initialize (C : in out Ctrl);
    --	procedure Adjust (C : in out Ctrl);
    --	procedure Finalize (C : in out Ctrl);
    --
    --	type Ctrlcomp is
    --	    record
    --		C1 : Ctrl := (Controlled with "xyz");
    --		C2 : Character;
    --	    end record;
    --
    --	X : Ctrlcomp;  -- Does not call Adjust by 7.6(17.1/1)
    --
    --	pragma Convention (Ada, Initialize); -- OK
    --	pragma Convention (Ada, Adjust); -- OK
    --	pragma Convention (Ada, Finalize); -- OK
    --end P9;
    --
    --
    --package P10 is
    --
    --	type Ctrl is new Controlled with
    --	    record
    --		C : String (1 .. 3);
    --	    end record;
    --
    --	procedure Initialize (C : in out Ctrl);
    --	procedure Adjust (C : in out Ctrl);
    --	procedure Finalize (C : in out Ctrl);
    --
    --    Foo : Ctrl := (Controlled with "xyz");
    --
    --	type Ctrlcomp is
    --	    record
    --		C1 : Ctrl := Foo;
    --		C2 : Character;
    --	    end record;
    --
    --	X : Ctrlcomp;
    --
    --	pragma Convention (Ada, Initialize); -- OK
    --	pragma Convention (Ada, Adjust); -- ERROR: Adjust is frozen
    --	pragma Convention (Ada, Finalize); -- OK
    --end P10;
    --
    --
    --package P11 is
    --
    --	type Ctrl is new Controlled with
    --	    record
    --		C : String (1 .. 3);
    --	    end record;
    --
    --	procedure Initialize (C : in out Ctrl);
    --	procedure Adjust (C : in out Ctrl);
    --	procedure Finalize (C : in out Ctrl);
    --
    --	type Nctrl is new Ctrl with null record;
    --
    --	X : Nctrl := (Ctrl with null record);
    --
    --	pragma Convention (Ada, Initialize); -- ERROR: Initialize is frozen
    --	pragma Convention (Ada, Adjust); -- OK
    --	pragma Convention (Ada, Finalize); -- OK
    --end P11;


    package P12 is

	type Rec is
	    record
		C1 : Float;
		C2 : String (1 .. 17);
	    end record;

	type Astream is access Ada.Streams.Root_Stream_Type'Class;
	S : Astream;

	function Input (Stream : access Ada.Streams.Root_Stream_Type'Class)
		       return Rec;

	for Rec'Input use Input;

	Z : Rec := Rec'Input (S);

	pragma Convention (Ada, Input); -- ERROR: Input is frozen

    end P12;


    package P13 is

	type Rec1 is
	    record
		C1 : Float;
		C2 : String (1 .. 17);
	    end record;

	type Astream is access Ada.Streams.Root_Stream_Type'Class;
	S : Astream;

	function Input (Stream : access Ada.Streams.Root_Stream_Type'Class)
		       return Rec1;

	for Rec1'Input use Input;

	type Rec2 is new Rec1; -- Inherits Rec1'Input.

	Z : Rec2 := Rec2'Input (S);

	pragma Convention (Ada, Input); -- ERROR: Input is frozen

    end P13;


    package P14 is

	type Rec is
	    record
		C1 : Float;
		C2 : String (1 .. 17);
	    end record;

	function Input (Stream : access Ada.Streams.Root_Stream_Type'Class)
		       return Rec;

	for Rec'Input use Input;

	pragma Convention (Ada, Input); -- OK

    end P14;


    package P15 is

	type Rec1 is
	    record
		C1 : Float;
		C2 : String (1 .. 17);
	    end record;

	type Astream is access Ada.Streams.Root_Stream_Type'Class;
	S : Astream;

	procedure Read (Stream : access Ada.Streams.Root_Stream_Type'Class;
		        Item : out Rec1);

	for Rec1'Read use Read;

	type Rec2 is
	    record
		C : Rec1;
	    end record;

	Z : Rec2 := Rec2'Input (S); -- Calls Rec2'Read which calls Rec1'Read.

	pragma Convention (Ada, Read); -- ERROR: Read is frozen

    end P15;


end BDE0009;

