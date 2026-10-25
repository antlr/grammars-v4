-- B8510010.A
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
--    Check that the restrictions on renaming components that depend on a
--    discriminant are enforced in the public and private parts of an instance.
--    Check that the restrictions on renaming components that depend on a
--    discriminant are enforced in the body of a generic unit.  Include
--    the decendant of an untagged generic formal derived type case.  (Defect
--    Report 8652/0017, Technical Corrigendum 8.5.1(5/1)).
--
-- TEST FILES:
--      This test consists of the following files:
--      -> B8510010.A
--         B8510011.A
--         B8510012.AM
--
-- PASS/FAIL CRITERIA:
--      Files B8510011.A and B8510012.AM contain errors. All
--      errors in these files must be detected to pass the test.
--
-- CHANGE HISTORY:
--    25 JAN 2001   PHL   Initial version.
--    29 JUN 2001   RLB   Reformatted for ACATS. Split files.
--
--!

package B851001_0 is
    pragma Elaborate_Body;

    type T1 (D1 : Boolean) is
	record
	    case D1 is
		when False =>
		    C1 : Integer;
		when True =>
		    C2 : Float;
	    end case;
	end record;

    type T2 (D2 : Boolean := True) is new T1 (D1 => D2);

    generic
	type F is new T1;
	X : in out F;
    package G1 is
	C1_Ren : Integer renames X.C1; -- OK.
    end G1;

    generic
	type F is new T1;
	X : in out F;
    package G2 is
    private
	C1_Ren : Integer renames X.C1; -- OK.
    end G2;

    generic
	type F is new T1;
	X : in out F;
    package G3 is
    end G3;

    generic
	type F is new T2;
	X : in out F;
    package G4 is
    end G4;

end B851001_0;

