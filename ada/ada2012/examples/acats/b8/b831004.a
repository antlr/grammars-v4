-- B831004.A
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
--    Check that an operation with the indicator of overriding is illegal
--    if it is primitive for a type derived from a generic formal type and
--    the operation does not inherit a homograph. (Part A: package
--    specifications)
--
--    Check that an operation with the indicator of not overriding is illegal
--    if it is primitive for a type derived from a generic formal type and
--    the operation inherits a homograph in either the generic or the instance.
--    (Part A: package specifications)
--
-- CHANGE HISTORY:
--    3 Jun 2004   PHL   Initial version.
--   16 Apr 2015   RLB   Renamed for issuance; added additional test cases.
--   22 Apr 2015   RLB   Corrected results for private operations in instances,
--                       to account for 12.3(18).
--
--!
package B831004 is

    package P is
        type T is tagged null record;
        procedure Q (X : out T);
        function G (X : T) return T;

        type E is (Oct, Nov, Dec);
        procedure R (X : in out E);
        function H (X : E) return E;
    end P;


    -- "overriding" for generic units.

    generic
        type F is tagged private;
    package G1A is
        type T is new F with null record;

        overriding
        procedure Q (X : out T);                           -- ERROR:

        overriding
        procedure P (X : in out T);                        -- ERROR:

    end G1A;

    generic
        type F is new P.T with private;
    package G1B is
        type T is new F with null record;

        overriding
        procedure Q (X : out T);                           -- OK.

        overriding
        function G (X : T) return T;                       -- OK.

    end G1B;

    generic
        type F is new P.T with private;
    package G1C is
        type T is new F with private;

        overriding
        procedure Q (X : out T);                           -- OK.

    private

        overriding
        function G (X : T) return T;                       -- OK.

        type T is new F with null record;
    end G1C;

    generic
        type F is new P.T with private;
    package G1D is
        type T is private;

        overriding
        procedure Q (X : out T);                           -- ERROR:

    private
        type T is new F with null record;

        overriding
        function G (X : T) return T;                       -- OK.

        overriding
        procedure R (X : in out T);                        -- ERROR:

    end G1D;

    generic
        type F is new P.T with private;
    package G1E is
    private
        type T is new F with null record;

        overriding
        function G (X : T) return T;                       -- OK.

        overriding
        function E (X : T) return T;                       -- ERROR:

    end G1E;

    generic
        type F is private;
    package G1F is
        type T is new F;

        overriding
        function H (X : T) return T;                       -- ERROR:

        overriding
        procedure R (X : in out T);                        -- ERROR:

    end G1F;

    generic
        type F is new P.E;
    package G1G is
        type T is new F;

        overriding
        function H (X : T) return T;                       -- OK.

        overriding
        function J (X : T) return T;                       -- ERROR:

        overriding
        procedure R (X : in out T);                        -- OK.

        overriding
        procedure Q (X : out T);                           -- ERROR:

    end G1G;

    generic
        type F is new P.E;
    package G1H is
        type T is private;

        overriding
        function H (X : T) return T;                       -- ERROR:

    private
        type T is new F;

        overriding
        function J (X : T) return T;                       -- ERROR:

        overriding
        procedure R (X : in out T);                        -- OK.

        overriding
        procedure Q (X : out T);                           -- ERROR:

    end G1H;


    -- "not overriding" for generic units.

    generic
        type F is tagged private;
    package G2A is
        type T is new F with null record;

        not overriding
        function G (X : T) return T;                       -- OK.

    end G2A;

    generic
        type F is new P.T with private;
    package G2B is
        type T is new F with null record;

        not overriding
        procedure Q (X : out T);                           -- ERROR:

        not overriding
        function G (X : T) return T;                       -- ERROR:

    end G2B;

    generic
        type F is new P.T with private;
    package G2C is
        type T is new F with private;

        not overriding
        function G (X : T) return T;                       -- ERROR:

    private

        not overriding
        procedure Q (X : out T);                           -- ERROR:

        type T is new F with null record;
    end G2C;

    generic
        type F is new P.T with private;
    package G2D is
        type T is private;

        not overriding
        function G (X : T) return T;                       -- ERROR:

    private
        type T is new F with null record;

        not overriding
        procedure Q (X : out T);                           -- ERROR:

    end G2D;

    generic
        type F is tagged private;
    package G2E is
        type T is new F with null record;

        not overriding
        function G (X : T) return T;                       -- OK.

        not overriding
        procedure Q (X : out T);                           -- OK.

    end G2E;

    generic
        type F is tagged private;
    package G2F is
        type T is tagged private;
    private

        type T is new F with null record;

        --not overriding
        --function G (X : T) return T;
        -- Declaring a function returning tagged type T is illegal here.

        not overriding
        procedure Q (X : out T);                           -- OK.

    end G2F;

    generic
        type F is private;
    package G2G is
        type T is new F;

        not overriding
        function H (X : T) return T;                       -- OK.

        not overriding
        procedure R (X : in out T);                        -- OK.

    end G2G;

    generic
        type F is private;
    package G2H is
        type T is private;
    private
        type T is new F;

        not overriding
        function H (X : T) return T;                       -- OK.

        not overriding
        procedure R (X : in out T);                        -- OK.

    end G2H;

    generic
        type F is new P.E;
    package G2I is
        type T is new F;

        not overriding
        function H (X : T) return T;                       -- ERROR:

        not overriding
        procedure R (X : in out T);                        -- ERROR:

    end G2I;

    generic
        type F is new P.E;
    package G2J is
        type T is private;

        not overriding
        function H (X : T) return T;                       -- ERROR:

        not overriding
        procedure R (X : in out T);                        -- ERROR:

    private
        type T is new F;
    end G2J;

    generic
        type F is new P.E;
    package G2K is
        type T is private;

    private
        type T is new F;

        not overriding
        function H (X : T) return T;                       -- ERROR:

        not overriding
        procedure R (X : in out T);                        -- ERROR:

    end G2K;


    -- Check that the "implicit contract" introduced by "not overriding" for
    -- generic units is enforced.

    package I1B is new G1B (P.T);                          -- OK.
    package I1C is new G1C (P.T);                          -- OK.
    package I2A is new G2A (P.T);                          -- ERROR:
    package I2E is new G2E (P.T);                          -- ERROR:
    package I2F is new G2F (P.T);                          -- OK.
    package I2G is new G2G (P.E);                          -- ERROR:
    package I2H is new G2H (P.E);                          -- OK.

    -- Note: I2F and I2H are OK, as 12.3(18) says that declarations in the
    -- private part of a generic specification are not overriding in an
    -- instance if they are not overriding in the generic. Thus 8.3.1(7/2)
    -- has no effect for "not overriding" in the private part.

end B831004;
