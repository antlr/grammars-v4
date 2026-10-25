-- B6700011.AM
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
--    See B6700010.A.
--
-- TEST DESCRIPTION:
--    See B6700010.A.
--
-- TEST FILES:
--      This test consists of the following files:
--         B6700010.A
--      -> B6700011.AM
--         B6700012.A
--         B6700013.A
--
-- CHANGE HISTORY:
--    19 Aug 2004   PHL   Initial version.
--    03 May 2007   RLB   Added test cases for additional kinds of completion.
--    18 Aug 2007   RLB   Fixed unit name.
--
--!

with B670001_0.Child;
procedure B670001 is

    generic
       type Priv is private;
       Val : in Priv;
    procedure Gen (X : out Priv);

    procedure Null1 (X : out Integer) is null;

    package Pak is
	procedure Null2 (X, Y : Boolean; Z : in out Duration) is null;
    private
	procedure Null3 (T : access Float) is null;
    end Pak;

    package body Pak is

	procedure Null3 (T : access Float) is                    -- ERROR:
	begin
	    T.all := 42.0;
	end Null3;

	procedure Null2 (X, Y : Boolean; Z : in out Duration) is -- ERROR:
	begin
	    if X and Y then
		Z := 1.0;
	    else
		Z := 4.0;
	    end if;
	end Null2;

    end Pak;

    procedure Gen (X : out Priv) is                              -- OK.
    begin
	X := Val;
    end Gen;

    procedure Null1 is new Gen (Integer, 42);                    -- ERROR:

begin
    null;
end B670001;

