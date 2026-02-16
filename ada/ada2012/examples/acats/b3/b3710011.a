-- B3710011.A
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
-- OBJECTIVE
--      See B3710010.A.
--
-- TEST FILES:
--      This test consists of the following files:
--         B3710010.A
--      -> B3710011.A
--         B3710012.A
--         B3710013.A
--         B3710014.AM
--
-- PASS/FAIL CRITERIA:
--      See B3710010.A.
--
-- CHANGE HISTORY:
--    18 Jan 2001   PHL   Initial version.
--    16 Mar 2001   RLB   Readied for release. Split files.
--    05 Jun 2001   RLB   Normalized "ERROR:" comments.
--    21 Mar 2007   RLB   Updated for Amendment 1 changes.
--    28 Apr 2008   RLB   Changed comments to reflect wording of AI05-0008 and
--                        AI05-0041.
--!

with B371001_0;
package body B371001_1.Child_1 is

    type Ptr3 is access all T;
    type Ptr4 is access T;

    -- A dereference of a general access type which designates T is not known
    -- to be constrained, because 3.4(23.3/3) does not apply as T is a definite
    -- type (as it has defaults for its discriminants), and 3.4(23.9/3)
    -- does not apply to general access types. Thus a discriminant constraint
    -- is illegal, as 3.7.1(7/3) requires the dereference to be known to be
    -- constrained. However, a dereference of a pool-specific access type
    -- which designates T is known to be constrained because 3.4(23.9/3),
    -- and therefore a discriminant constraint is legal.

    P1 : Ptr1 (0) := B371001_0.Y (2)'Access; -- ERROR:
    subtype S2 is Ptr2 (2);                  -- ERROR:
    subtype S3 is Ptr3 (3);                  -- ERROR:
    P4 : Ptr4 (4);                           -- OK (not a general access type).

begin
    B371001_0.X := (others => (D => 4));
end B371001_1.Child_1;

