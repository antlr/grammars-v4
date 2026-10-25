-- B392011.A
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
--     DISCLOSED ARE AS IS.  THE GOVERNMENT MAKES NO EXPRESS OR IMPLIED
--     WARRANTY AS TO ANY MATTER WHATSOEVER, INCLUDING THE CONDITIONS OF THE
--     SOFTWARE, DOCUMENTATION OR OTHER INFORMATION RELEASED, MADE AVAILABLE
--     OR DISCLOSED, OR THE OWNERSHIP, MERCHANTABILITY, OR FITNESS FOR A
--     PARTICULAR PURPOSE OF SAID MATERIAL.
--*
--
-- OBJECTIVE:
--    Check that if the expected type for an expression is an anonymous
--    access-to-specific tagged type, then the object designated by the
--    expression shall not be dynamically tagged unless it is a controlling
--    operand in a call on a dispatching operation.  This test only addresses
--    the expressions that were made dynamically tagged by the language change,
--    i.e., those of the form X'Access (where X is of a class-wide type) and
--    new T'Class'(...). For other expressions, see test B392004. (Defect
--    Report 8652/0010, Technical Corrigendum 3.9.2(9/1)).
--
-- CHANGE HISTORY:
--    19 JAN 2001   PHL   Initial version.
--    29 JUN 2001   RLB   Reformatted for ACATS.
--
--!
procedure B392011 is

    type T is tagged
	record
	    C : Character;
	end record;

    type Nt is new T with
	record
	    Nc : Float;
	end record;

    type Nnt is new Nt with
	record
	    Nnc : Duration;
	end record;

    subtype S is T'Class;

    A : T'Class := T'Class (Nnt'(C => 'a', Nc => 2.0, Nnc => 3.0));
    B : T'Class := T'Class (Nt'(C => 'b', Nc => 5.0));

    X : aliased T'Class := T'Class (A);

    type R (D : access T) is limited null record;

    Y1 : R (D => X'Access);                     -- ERROR:
    Y2 : R (D => new T'Class'(B));              -- ERROR:

    procedure P1 (Z : access T := X'Access) is  -- ERROR:
    begin
	null;
    end P1;

    procedure P2 (Z : access T := new S'(A)) is -- ERROR:
    begin
	null;
    end P2;

begin

    declare
	procedure Q (Z : access T) is
	begin
	    null;
	end Q;
    begin
	Q (X'Access);                           -- ERROR:
	Q (new S'(B));                          -- ERROR:
    end;

end B392011;
