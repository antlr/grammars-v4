-- -----------------------------------------------------------------------------
--
-- Copyright 1995 by IEEE. All rights reserved.
--
-- This source file is considered by the IEEE to be an essential part of the use
-- of the standard 1076.3 and as such may be distributed without change, except
-- as permitted by the standard. This source file may not be sold or distributed
-- for profit. This package may be modified to include additional data required
-- by tools, but must in no way change the external interfaces or simulation
-- behaviour of the description. It is permissible to add comments and/or
-- attributes to the package declarations, but not to change or delete any
-- original lines of the approved package declaration. The package body may be
-- changed only in accordance with the terms of clauses 7.1 and 7.2 of the
-- standard.
--
-- Title      : Standard VHDL Synthesis Package (1076.3, NUMERIC_BIT)
--
-- Library    : This package shall be compiled into a library symbolically
--            : named IEEE.
--
-- Developers : IEEE DASC Synthesis Working Group, PAR 1076.3
--
-- Purpose    : This package defines numeric types and arithmetic functions
--            : for use with synthesis tools. Two numeric types are defined:
--            : -- > UNSIGNED: represents an UNSIGNED number in vector form
--            : -- > SIGNED: represents a SIGNED number in vector form
--            : The base element type is type BIT.
--            : The leftmost bit is treated as the most significant bit.
--            : Signed vectors are represented in two's complement form.
--            : This package contains overloaded arithmetic operators on
--            : the SIGNED and UNSIGNED types. The package also contains
--            : useful type conversions functions, clock detection
--            : functions, and other utility functions.
--            :
--            : If any argument to a function is a null array, a null array is
--            : returned (exceptions, if any, are noted individually).
--
-- Limitation :
--
-- Note       : No declarations or definitions shall be included in,
--            : or excluded from this package. The "package declaration"
--            : defines the types, subtypes and declarations of
--            : NUMERIC_BIT. The NUMERIC_BIT package body shall be
--            : considered the formal definition of the semantics of
--            : this package. Tool developers may choose to implement
--            : the package body in the most efficient manner available
--            : to them.
--            :
-- -----------------------------------------------------------------------------
-- Version    : 2.4
-- Date       : 12 April 1995
-- -----------------------------------------------------------------------------

--==============================================================================
--======================= Package Body =========================================
--==============================================================================

package body NUMERIC_BIT is

  -- null range array constants

  constant NAU: UNSIGNED(0 downto 1) := (others => '0');
  constant NAS: SIGNED(0 downto 1) := (others => '0');

  -- implementation controls

  constant NO_WARNING: BOOLEAN := FALSE; -- default to emit warnings

  --=========================Local Subprograms =================================

  function MAX (LEFT, RIGHT: INTEGER) return INTEGER is
  begin
    if LEFT > RIGHT then return LEFT;
    else return RIGHT;
    end if;
  end MAX;

  function MIN (LEFT, RIGHT: INTEGER) return INTEGER is
  begin
    if LEFT < RIGHT then return LEFT;
    else return RIGHT;
    end if;
  end MIN;

  function SIGNED_NUM_BITS (ARG: INTEGER) return NATURAL is
    variable NBITS: NATURAL;
    variable N: NATURAL;
  begin
    if ARG >= 0 then
      N := ARG;
    else
      N := -(ARG+1);
    end if;
    NBITS := 1;
    while N > 0 loop
      NBITS := NBITS+1;
      N := N / 2;
    end loop;
    return NBITS;
  end SIGNED_NUM_BITS;

  function UNSIGNED_NUM_BITS (ARG: NATURAL) return NATURAL is
    variable NBITS: NATURAL;
    variable N: NATURAL;
  begin
    N := ARG;
    NBITS := 1;
    while N > 1 loop
      NBITS := NBITS+1;
      N := N / 2;
    end loop;
    return NBITS;
  end UNSIGNED_NUM_BITS;

  ------------------------------------------------------------------------------
  -- this internal function computes the addition of two UNSIGNED
  -- with input carry
  -- * the two arguments are of the same length

  function ADD_UNSIGNED (L, R: UNSIGNED; C: BIT) return UNSIGNED is
    constant L_LEFT: INTEGER := L'LENGTH-1;
    alias XL: UNSIGNED(L_LEFT downto 0) is L;
    alias XR: UNSIGNED(L_LEFT downto 0) is R;
    variable RESULT: UNSIGNED(L_LEFT downto 0);
    variable CBIT: BIT := C;
  begin
    for I in 0 to L_LEFT loop
      RESULT(I) := CBIT xor XL(I) xor XR(I);
      CBIT := (CBIT and XL(I)) or (CBIT and XR(I)) or (XL(I) and XR(I));
    end loop;
    return RESULT;
  end ADD_UNSIGNED;

  -- this internal function computes the addition of two SIGNED
  -- with input carry
  -- * the two arguments are of the same length

  function ADD_SIGNED (L, R: SIGNED; C: BIT) return SIGNED is
    constant L_LEFT: INTEGER := L'LENGTH-1;
    alias XL: SIGNED(L_LEFT downto 0) is L;
    alias XR: SIGNED(L_LEFT downto 0) is R;
    variable RESULT: SIGNED(L_LEFT downto 0);
    variable CBIT: BIT := C;
  begin
    for I in 0 to L_LEFT loop
      RESULT(I) := CBIT xor XL(I) xor XR(I);
      CBIT := (CBIT and XL(I)) or (CBIT and XR(I)) or (XL(I) and XR(I));
    end loop;
    return RESULT;
  end ADD_SIGNED;

  ------------------------------------------------------------------------------

  -- this internal procedure computes UNSIGNED division
  -- giving the quotient and remainder.
  procedure DIVMOD (NUM, XDENOM: UNSIGNED; XQUOT, XREMAIN: out UNSIGNED) is
    variable TEMP: UNSIGNED(NUM'LENGTH downto 0);
    variable QUOT: UNSIGNED(MAX(NUM'LENGTH, XDENOM'LENGTH)-1 downto 0);
    alias DENOM: UNSIGNED(XDENOM'LENGTH-1 downto 0) is XDENOM;
    variable TOPBIT: INTEGER;
  begin
    TEMP := "0"&NUM;
    QUOT := (others => '0');
    TOPBIT := -1;
    for J in DENOM'RANGE loop
      if DENOM(J)='1' then
        TOPBIT := J;
        exit;
      end if;
    end loop;
    assert TOPBIT >= 0 report "DIV, MOD, or REM by zero" severity ERROR;

    for J in NUM'LENGTH-(TOPBIT+1) downto 0 loop
      if TEMP(TOPBIT+J+1 downto J) >= "0"&DENOM(TOPBIT downto 0) then
        TEMP(TOPBIT+J+1 downto J) := (TEMP(TOPBIT+J+1 downto J))
            -("0"&DENOM(TOPBIT downto 0));
        QUOT(J) := '1';
      end if;
      assert TEMP(TOPBIT+J+1)='0'
          report "internal error in the division algorithm"
          severity ERROR;
    end loop;
    XQUOT := RESIZE(QUOT, XQUOT'LENGTH);
    XREMAIN := RESIZE(TEMP, XREMAIN'LENGTH);
  end DIVMOD;

  -----------------Local Subprograms - shift/rotate ops-------------------------

  function XSLL (ARG: BIT_VECTOR; COUNT: NATURAL) return BIT_VECTOR is
    constant ARG_L: INTEGER := ARG'LENGTH-1;
    alias XARG: BIT_VECTOR(ARG_L downto 0) is ARG;
    variable RESULT: BIT_VECTOR(ARG_L downto 0) := (others => '0');
  begin
    if COUNT <= ARG_L then
      RESULT(ARG_L downto COUNT) := XARG(ARG_L-COUNT downto 0);
    end if;
    return RESULT;
  end XSLL;

  function XSRL (ARG: BIT_VECTOR; COUNT: NATURAL) return BIT_VECTOR is
    constant ARG_L: INTEGER := ARG'LENGTH-1;
    alias XARG: BIT_VECTOR(ARG_L downto 0) is ARG;
    variable RESULT: BIT_VECTOR(ARG_L downto 0) := (others => '0');
  begin
    if COUNT <= ARG_L then
      RESULT(ARG_L-COUNT downto 0) := XARG(ARG_L downto COUNT);
    end if;
    return RESULT;
  end XSRL;

  function XSRA (ARG: BIT_VECTOR; COUNT: NATURAL) return BIT_VECTOR is
    constant ARG_L: INTEGER := ARG'LENGTH-1;
    alias XARG: BIT_VECTOR(ARG_L downto 0) is ARG;
    variable RESULT: BIT_VECTOR(ARG_L downto 0);
    variable XCOUNT: NATURAL := COUNT;
  begin
    if ((ARG'LENGTH <= 1) or (XCOUNT = 0)) then return ARG;
    else
      if (XCOUNT > ARG_L) then XCOUNT := ARG_L;
      end if;
      RESULT(ARG_L-XCOUNT downto 0) := XARG(ARG_L downto XCOUNT);
      RESULT(ARG_L downto (ARG_L - XCOUNT + 1)) := (others => XARG(ARG_L));
    end if;
    return RESULT;
  end XSRA;

  function XROL (ARG: BIT_VECTOR; COUNT: NATURAL) return BIT_VECTOR is
    constant ARG_L: INTEGER := ARG'LENGTH-1;
    alias XARG: BIT_VECTOR(ARG_L downto 0) is ARG;
    variable RESULT: BIT_VECTOR(ARG_L downto 0) := XARG;
    variable COUNTM: INTEGER;
  begin
    COUNTM := COUNT mod (ARG_L + 1);
    if COUNTM /= 0 then
      RESULT(ARG_L downto COUNTM) := XARG(ARG_L-COUNTM downto 0);
      RESULT(COUNTM-1 downto 0) := XARG(ARG_L downto ARG_L-COUNTM+1);
    end if;
    return RESULT;
  end XROL;

  function XROR (ARG: BIT_VECTOR; COUNT: NATURAL) return BIT_VECTOR is
    constant ARG_L: INTEGER := ARG'LENGTH-1;
    alias XARG: BIT_VECTOR(ARG_L downto 0) is ARG;
    variable RESULT: BIT_VECTOR(ARG_L downto 0) := XARG;
    variable COUNTM: INTEGER;
  begin
    COUNTM := COUNT mod (ARG_L + 1);
    if COUNTM /= 0 then
      RESULT(ARG_L-COUNTM downto 0) := XARG(ARG_L downto COUNTM);
      RESULT(ARG_L downto ARG_L-COUNTM+1) := XARG(COUNTM-1 downto 0);
    end if;
    return RESULT;
  end XROR;

  ---------------- Local Subprograms - Relational Operators --------------------

  -- General "=" for UNSIGNED vectors, same length
  --
  function UNSIGNED_EQUAL (L, R: UNSIGNED) return BOOLEAN is
  begin
    return BIT_VECTOR(L) = BIT_VECTOR(R);
  end UNSIGNED_EQUAL;

  --
  -- General "=" for SIGNED vectors, same length
  --
  function SIGNED_EQUAL (L, R: SIGNED) return BOOLEAN is
  begin
    return BIT_VECTOR(L) = BIT_VECTOR(R);
  end SIGNED_EQUAL;

  --
  -- General "<" for UNSIGNED vectors, same length
  --
  function UNSIGNED_LESS (L, R: UNSIGNED) return BOOLEAN is
  begin
    return BIT_VECTOR(L) < BIT_VECTOR(R);
  end UNSIGNED_LESS;

  --
  -- General "<" function for SIGNED vectors, same length
  --
  function SIGNED_LESS (L, R: SIGNED) return BOOLEAN is
    -- Need aliases to assure index direction
    variable INTERN_L: SIGNED(0 to L'LENGTH-1);
    variable INTERN_R: SIGNED(0 to R'LENGTH-1);
  begin
    INTERN_L := L;
    INTERN_R := R;
    INTERN_L(0) := not INTERN_L(0);
    INTERN_R(0) := not INTERN_R(0);
    return BIT_VECTOR(INTERN_L) < BIT_VECTOR(INTERN_R);
  end SIGNED_LESS;

  --
  -- General "<=" function for UNSIGNED vectors, same length
  --
  function UNSIGNED_LESS_OR_EQUAL (L, R: UNSIGNED) return BOOLEAN is
  begin
    return BIT_VECTOR(L) <= BIT_VECTOR(R);
  end UNSIGNED_LESS_OR_EQUAL;

  --
  -- General "<=" function for SIGNED vectors, same length
  --
  function SIGNED_LESS_OR_EQUAL (L, R: SIGNED) return BOOLEAN is
    -- Need aliases to assure index direction
    variable INTERN_L: SIGNED(0 to L'LENGTH-1);
    variable INTERN_R: SIGNED(0 to R'LENGTH-1);
  begin
    INTERN_L := L;
    INTERN_R := R;
    INTERN_L(0) := not INTERN_L(0);
    INTERN_R(0) := not INTERN_R(0);
    return BIT_VECTOR(INTERN_L) <= BIT_VECTOR(INTERN_R);
  end SIGNED_LESS_OR_EQUAL;

  --====================== Exported Functions ==================================

  -- Id: A.1
  function "abs" (ARG: SIGNED) return SIGNED is
    constant ARG_LEFT: INTEGER := ARG'LENGTH-1;
    variable RESULT: SIGNED(ARG_LEFT downto 0);
  begin
    if ARG'LENGTH < 1 then return NAS;
    end if;
    RESULT := ARG;
    if RESULT(RESULT'LEFT) = '1' then
      RESULT := -RESULT;
    end if;
    return RESULT;
  end "abs";

  -- Id: A.2
  function "-" (ARG: SIGNED) return SIGNED is
    constant ARG_LEFT: INTEGER := ARG'LENGTH-1;
    alias XARG: SIGNED(ARG_LEFT downto 0) is ARG;
    variable RESULT: SIGNED(ARG_LEFT downto 0);
    variable CBIT: BIT := '1';
  begin
    if ARG'LENGTH < 1 then return NAS;
    end if;
    for I in 0 to RESULT'LEFT loop
      RESULT(I) := not(XARG(I)) xor CBIT;
      CBIT := CBIT and not(XARG(I));
    end loop;
    return RESULT;
  end "-";

  --============================================================================

  -- Id: A.3
  function "+" (L, R: UNSIGNED) return UNSIGNED is
    constant L_LEFT: INTEGER := L'LENGTH-1;
    constant R_LEFT: INTEGER := R'LENGTH-1;
    constant SIZE: NATURAL := MAX(L'LENGTH, R'LENGTH);
  begin
    if ((L'LENGTH < 1) or (R'LENGTH < 1)) then return NAU;
    end if;
    return ADD_UNSIGNED(RESIZE(L, SIZE), RESIZE(R, SIZE), '0');
  end "+";

  -- Id: A.4
  function "+" (L, R: SIGNED) return SIGNED is
    constant L_LEFT: INTEGER := L'LENGTH-1;
    constant R_LEFT: INTEGER := R'LENGTH-1;
    constant SIZE: NATURAL := MAX(L'LENGTH, R'LENGTH);
  begin
    if ((L'LENGTH < 1) or (R'LENGTH < 1)) then return NAS;
    end if;
    return ADD_SIGNED(RESIZE(L, SIZE), RESIZE(R, SIZE), '0');
  end "+";

  -- Id: A.5
  function "+" (L: UNSIGNED; R: NATURAL) return UNSIGNED is
  begin
    return L + TO_UNSIGNED(R, L'LENGTH);
  end "+";

  -- Id: A.6
  function "+" (L: NATURAL; R: UNSIGNED) return UNSIGNED is
  begin
    return TO_UNSIGNED(L, R'LENGTH) + R;
  end "+";

  -- Id: A.7
  function "+" (L: SIGNED; R: INTEGER) return SIGNED is
  begin
    return L + TO_SIGNED(R, L'LENGTH);
  end "+";

  -- Id: A.8
  function "+" (L: INTEGER; R: SIGNED) return SIGNED is
  begin
    return TO_SIGNED(L, R'LENGTH) + R;
  end "+";

  --============================================================================

  -- Id: A.9
  function "-" (L, R: UNSIGNED) return UNSIGNED is
    constant L_LEFT: INTEGER := L'LENGTH-1;
    constant R_LEFT: INTEGER := R'LENGTH-1;
    constant SIZE: NATURAL := MAX(L'LENGTH, R'LENGTH);
  begin
    if ((L'LENGTH < 1) or (R'LENGTH < 1)) then return NAU;
    end if;
    return ADD_UNSIGNED(RESIZE(L, SIZE),
        not(RESIZE(R, SIZE)),
        '1');
  end "-";

  -- Id: A.10
  function "-" (L, R: SIGNED) return SIGNED is
    constant L_LEFT: INTEGER := L'LENGTH-1;
    constant R_LEFT: INTEGER := R'LENGTH-1;
    constant SIZE: NATURAL := MAX(L'LENGTH, R'LENGTH);
  begin
    if ((L'LENGTH < 1) or (R'LENGTH < 1)) then return NAS;
    end if;
    return ADD_SIGNED(RESIZE(L, SIZE),
        not(RESIZE(R, SIZE)),
        '1');
  end "-";

  -- Id: A.11
  function "-" (L: UNSIGNED; R: NATURAL) return UNSIGNED is
  begin
    return L - TO_UNSIGNED(R, L'LENGTH);
  end "-";

  -- Id: A.12
  function "-" (L: NATURAL; R: UNSIGNED) return UNSIGNED is
  begin
    return TO_UNSIGNED(L, R'LENGTH) - R;
  end "-";

  -- Id: A.13
  function "-" (L: SIGNED; R: INTEGER) return SIGNED is
  begin
    return L - TO_SIGNED(R, L'LENGTH);
  end "-";

  -- Id: A.14
  function "-" (L: INTEGER; R: SIGNED) return SIGNED is
  begin
    return TO_SIGNED(L, R'LENGTH) - R;
  end "-";

  --============================================================================

  -- Id: A.15
  function "*" (L, R: UNSIGNED) return UNSIGNED is
    constant L_LEFT: INTEGER := L'LENGTH-1;
    constant R_LEFT: INTEGER := R'LENGTH-1;
    alias XL: UNSIGNED(L_LEFT downto 0) is L;
    alias XR: UNSIGNED(R_LEFT downto 0) is R;
    variable RESULT: UNSIGNED((L'LENGTH+R'LENGTH-1) downto 0) := (others => '0');
    variable ADVAL: UNSIGNED((L'LENGTH+R'LENGTH-1) downto 0);
  begin
    if ((L'LENGTH < 1) or (R'LENGTH < 1)) then return NAU;
    end if;
    ADVAL := RESIZE(XR, RESULT'LENGTH);
    for I in 0 to L_LEFT loop
      if XL(I)='1' then RESULT := RESULT + ADVAL;
      end if;
      ADVAL := SHIFT_LEFT(ADVAL, 1);
    end loop;
    return RESULT;
  end "*";

  -- Id: A.16
  function "*" (L, R: SIGNED) return SIGNED is
    constant L_LEFT: INTEGER := L'LENGTH-1;
    constant R_LEFT: INTEGER := R'LENGTH-1;
    variable XL: SIGNED(L_LEFT downto 0);
    variable XR: SIGNED(R_LEFT downto 0);
    variable RESULT: SIGNED((L_LEFT+R_LEFT+1) downto 0) := (others => '0');
    variable ADVAL: SIGNED((L_LEFT+R_LEFT+1) downto 0);
  begin
    if ((L_LEFT < 0) or (R_LEFT < 0)) then return NAS;
    end if;
    XL := L;
    XR := R;
    ADVAL := RESIZE(XR, RESULT'LENGTH);
    for I in 0 to L_LEFT-1 loop
      if XL(I)='1' then RESULT := RESULT + ADVAL;
      end if;
      ADVAL := SHIFT_LEFT(ADVAL, 1);
    end loop;
    if XL(L_LEFT)='1' then
      RESULT := RESULT - ADVAL;
    end if;
    return RESULT;
  end "*";

  -- Id: A.17
  function "*" (L: UNSIGNED; R: NATURAL) return UNSIGNED is
  begin
    return L * TO_UNSIGNED(R, L'LENGTH);
  end "*";

  -- Id: A.18
  function "*" (L: NATURAL; R: UNSIGNED) return UNSIGNED is
  begin
    return TO_UNSIGNED(L, R'LENGTH) * R;
  end "*";

  -- Id: A.19
  function "*" (L: SIGNED; R: INTEGER) return SIGNED is
  begin
    return L * TO_SIGNED(R, L'LENGTH);
  end "*";

  -- Id: A.20
  function "*" (L: INTEGER; R: SIGNED) return SIGNED is
  begin
    return TO_SIGNED(L, R'LENGTH) * R;
  end "*";

  --============================================================================

  -- Id: A.21
  function "/" (L, R: UNSIGNED) return UNSIGNED is
    variable FQUOT: UNSIGNED(L'LENGTH-1 downto 0);
    variable FREMAIN: UNSIGNED(R'LENGTH-1 downto 0);
  begin
    if ((L'LENGTH < 1) or (R'LENGTH < 1)) then return NAU;
    end if;
    DIVMOD(L, R, FQUOT, FREMAIN);
    return FQUOT;
  end "/";

  -- Id: A.22
  function "/" (L, R: SIGNED) return SIGNED is
    variable FQUOT: UNSIGNED(L'LENGTH-1 downto 0);
    variable FREMAIN: UNSIGNED(R'LENGTH-1 downto 0);
    variable XNUM: UNSIGNED(L'LENGTH-1 downto 0);
    variable XDENOM: UNSIGNED(R'LENGTH-1 downto 0);
    variable QNEG: BOOLEAN := FALSE;
  begin
    if ((L'LENGTH < 1) or (R'LENGTH < 1)) then return NAS;
    end if;
    if L(L'LEFT)='1' then
      XNUM := UNSIGNED(-L);
      QNEG := TRUE;
    else
      XNUM := UNSIGNED(L);
    end if;
    if R(R'LEFT)='1' then
      XDENOM := UNSIGNED(-R);
      QNEG := not QNEG;
    else
      XDENOM := UNSIGNED(R);
    end if;
    DIVMOD(XNUM, XDENOM, FQUOT, FREMAIN);
    if QNEG then FQUOT := "0"-FQUOT;
    end if;
    return SIGNED(FQUOT);
  end "/";

  -- Id: A.23
  function "/" (L: UNSIGNED; R: NATURAL) return UNSIGNED is
    constant R_LENGTH: NATURAL := MAX(L'LENGTH, UNSIGNED_NUM_BITS(R));
    variable XR, QUOT: UNSIGNED(R_LENGTH-1 downto 0);
  begin
    if (L'LENGTH < 1) then return NAU;
    end if;
    if (R_LENGTH > L'LENGTH) then
      QUOT := (others => '0');
      return RESIZE(QUOT, L'LENGTH);
    end if;
    XR := TO_UNSIGNED(R, R_LENGTH);
    QUOT := RESIZE((L / XR), QUOT'LENGTH);
    return RESIZE(QUOT, L'LENGTH);
  end "/";

  -- Id: A.24
  function "/" (L: NATURAL; R: UNSIGNED) return UNSIGNED is
    constant L_LENGTH: NATURAL := MAX(UNSIGNED_NUM_BITS(L), R'LENGTH);
    variable XL, QUOT: UNSIGNED(L_LENGTH-1 downto 0);
  begin
    if (R'LENGTH < 1) then return NAU;
    end if;
    XL := TO_UNSIGNED(L, L_LENGTH);
    QUOT := RESIZE((XL / R), QUOT'LENGTH);
    if L_LENGTH > R'LENGTH
        and QUOT(L_LENGTH-1 downto R'LENGTH)
        /= (L_LENGTH-1 downto R'LENGTH => '0')
        then
      assert NO_WARNING report "NUMERIC_BIT.""/"": Quotient Truncated"
          severity WARNING;
    end if;
    return RESIZE(QUOT, R'LENGTH);
  end "/";

  -- Id: A.25
  function "/" (L: SIGNED; R: INTEGER) return SIGNED is
    constant R_LENGTH: NATURAL := MAX(L'LENGTH, SIGNED_NUM_BITS(R));
    variable XR, QUOT: SIGNED(R_LENGTH-1 downto 0);
  begin
    if (L'LENGTH < 1) then return NAS;
    end if;
    if (R_LENGTH > L'LENGTH) then
      QUOT := (others => '0');
      return RESIZE(QUOT, L'LENGTH);
    end if;
    XR := TO_SIGNED(R, R_LENGTH);
    QUOT := RESIZE((L / XR), QUOT'LENGTH);
    return RESIZE(QUOT, L'LENGTH);
  end "/";

  -- Id: A.26
  function "/" (L: INTEGER; R: SIGNED) return SIGNED is
    constant L_LENGTH: NATURAL := MAX(SIGNED_NUM_BITS(L), R'LENGTH);
    variable XL, QUOT: SIGNED(L_LENGTH-1 downto 0);
  begin
    if (R'LENGTH < 1) then return NAS;
    end if;
    XL := TO_SIGNED(L, L_LENGTH);
    QUOT := RESIZE((XL / R), QUOT'LENGTH);
    if L_LENGTH > R'LENGTH and QUOT(L_LENGTH-1 downto R'LENGTH)
        /= (L_LENGTH-1 downto R'LENGTH => QUOT(R'LENGTH-1))
        then
      assert NO_WARNING report "NUMERIC_BIT.""/"": Quotient Truncated"
          severity WARNING;
    end if;
    return RESIZE(QUOT, R'LENGTH);
  end "/";

  --============================================================================

  -- Id: A.27
  function "rem" (L, R: UNSIGNED) return UNSIGNED is
    variable FQUOT: UNSIGNED(L'LENGTH-1 downto 0);
    variable FREMAIN: UNSIGNED(R'LENGTH-1 downto 0);
  begin
    if ((L'LENGTH < 1) or (R'LENGTH < 1)) then return NAU;
    end if;
    DIVMOD(L, R, FQUOT, FREMAIN);
    return FREMAIN;
  end "rem";

  -- Id: A.28
  function "rem" (L, R: SIGNED) return SIGNED is
    variable FQUOT: UNSIGNED(L'LENGTH-1 downto 0);
    variable FREMAIN: UNSIGNED(R'LENGTH-1 downto 0);
    variable XNUM: UNSIGNED(L'LENGTH-1 downto 0);
    variable XDENOM: UNSIGNED(R'LENGTH-1 downto 0);
    variable RNEG: BOOLEAN := FALSE;
  begin
    if ((L'LENGTH < 1) or (R'LENGTH < 1)) then return NAS;
    end if;
    if L(L'LEFT)='1' then
      XNUM := UNSIGNED(-L);
      RNEG := TRUE;
    else
      XNUM := UNSIGNED(L);
    end if;
    if R(R'LEFT)='1' then
      XDENOM := UNSIGNED(-R);
    else
      XDENOM := UNSIGNED(R);
    end if;
    DIVMOD(XNUM, XDENOM, FQUOT, FREMAIN);
    if RNEG then
      FREMAIN := "0"-FREMAIN;
    end if;
    return SIGNED(FREMAIN);
  end "rem";

  -- Id: A.29
  function "rem" (L: UNSIGNED; R: NATURAL) return UNSIGNED is
    constant R_LENGTH: NATURAL := MAX(L'LENGTH, UNSIGNED_NUM_BITS(R));
    variable XR, XREM: UNSIGNED(R_LENGTH-1 downto 0);
  begin
    if (L'LENGTH < 1) then return NAU;
    end if;
    XR := TO_UNSIGNED(R, R_LENGTH);
    XREM := RESIZE((L rem XR), XREM'LENGTH);
    if R_LENGTH > L'LENGTH and XREM(R_LENGTH-1 downto L'LENGTH)
        /= (R_LENGTH-1 downto L'LENGTH => '0')
        then
      assert NO_WARNING report "NUMERIC_BIT.""rem"": Remainder Truncated"
          severity WARNING;
    end if;
    return RESIZE(XREM, L'LENGTH);
  end "rem";

  -- Id: A.30
  function "rem" (L: NATURAL; R: UNSIGNED) return UNSIGNED is
    constant L_LENGTH: NATURAL := MAX(UNSIGNED_NUM_BITS(L), R'LENGTH);
    variable XL, XREM: UNSIGNED(L_LENGTH-1 downto 0);
  begin
    if (R'LENGTH < 1) then return NAU;
    end if;
    XL := TO_UNSIGNED(L, L_LENGTH);
    XREM := RESIZE((XL rem R), XREM'LENGTH);
    if L_LENGTH > R'LENGTH and XREM(L_LENGTH-1 downto R'LENGTH)
        /= (L_LENGTH-1 downto R'LENGTH => '0')
        then
      assert NO_WARNING report "NUMERIC_BIT.""rem"": Remainder Truncated"
          severity WARNING;
    end if;
    return RESIZE(XREM, R'LENGTH);
  end "rem";

  -- Id: A.31
  function "rem" (L: SIGNED; R: INTEGER) return SIGNED is
    constant R_LENGTH: NATURAL := MAX(L'LENGTH, SIGNED_NUM_BITS(R));
    variable XR, XREM: SIGNED(R_LENGTH-1 downto 0);
  begin
    if (L'LENGTH < 1) then return NAS;
    end if;
    XR := TO_SIGNED(R, R_LENGTH);
    XREM := RESIZE((L rem XR), XREM'LENGTH);
    if R_LENGTH > L'LENGTH and XREM(R_LENGTH-1 downto L'LENGTH)
        /= (R_LENGTH-1 downto L'LENGTH => XREM(L'LENGTH-1))
        then
      assert NO_WARNING report "NUMERIC_BIT.""rem"": Remainder Truncated"
          severity WARNING;
    end if;
    return RESIZE(XREM, L'LENGTH);
  end "rem";

  -- Id: A.32
  function "rem" (L: INTEGER; R: SIGNED) return SIGNED is
    constant L_LENGTH: NATURAL := MAX(SIGNED_NUM_BITS(L), R'LENGTH);
    variable XL, XREM: SIGNED(L_LENGTH-1 downto 0);
  begin
    if (R'LENGTH < 1) then return NAS;
    end if;
    XL := TO_SIGNED(L, L_LENGTH);
    XREM := RESIZE((XL rem R), XREM'LENGTH);
    if L_LENGTH > R'LENGTH and XREM(L_LENGTH-1 downto R'LENGTH)
        /= (L_LENGTH-1 downto R'LENGTH => XREM(R'LENGTH-1))
        then
      assert NO_WARNING report "NUMERIC_BIT.""rem"": Remainder Truncated"
          severity WARNING;
    end if;
    return RESIZE(XREM, R'LENGTH);
  end "rem";

  --============================================================================

  -- Id: A.33
  function "mod" (L, R: UNSIGNED) return UNSIGNED is
    variable FQUOT: UNSIGNED(L'LENGTH-1 downto 0);
    variable FREMAIN: UNSIGNED(R'LENGTH-1 downto 0);
  begin
    if ((L'LENGTH < 1) or (R'LENGTH < 1)) then return NAU;
    end if;
    DIVMOD(L, R, FQUOT, FREMAIN);
    return FREMAIN;
  end "mod";

  -- Id: A.34
  function "mod" (L, R: SIGNED) return SIGNED is
    variable FQUOT: UNSIGNED(L'LENGTH-1 downto 0);
    variable FREMAIN: UNSIGNED(R'LENGTH-1 downto 0);
    variable XNUM: UNSIGNED(L'LENGTH-1 downto 0);
    variable XDENOM: UNSIGNED(R'LENGTH-1 downto 0);
    variable RNEG: BOOLEAN := FALSE;
  begin
    if ((L'LENGTH < 1) or (R'LENGTH < 1)) then return NAS;
    end if;
    if L(L'LEFT)='1' then
      XNUM := UNSIGNED(-L);
    else
      XNUM := UNSIGNED(L);
    end if;
    if R(R'LEFT)='1' then
      XDENOM := UNSIGNED(-R);
      RNEG := TRUE;
    else
      XDENOM := UNSIGNED(R);
    end if;
    DIVMOD(XNUM, XDENOM, FQUOT, FREMAIN);
    if RNEG and L(L'LEFT)='1' then
      FREMAIN := "0"-FREMAIN;
      elsif RNEG and FREMAIN/="0" then
          FREMAIN := FREMAIN-XDENOM;
      elsif L(L'LEFT)='1' and FREMAIN/="0" then
          FREMAIN := XDENOM-FREMAIN;
    end if;
    return SIGNED(FREMAIN);
  end "mod";

  -- Id: A.35
  function "mod" (L: UNSIGNED; R: NATURAL) return UNSIGNED is
    constant R_LENGTH: NATURAL := MAX(L'LENGTH, UNSIGNED_NUM_BITS(R));
    variable XR, XREM: UNSIGNED(R_LENGTH-1 downto 0);
  begin
    if (L'LENGTH < 1) then return NAU;
    end if;
    XR := TO_UNSIGNED(R, R_LENGTH);
    XREM := RESIZE((L mod XR), XREM'LENGTH);
    if R_LENGTH > L'LENGTH and XREM(R_LENGTH-1 downto L'LENGTH)
        /= (R_LENGTH-1 downto L'LENGTH => '0')
        then
      assert NO_WARNING report "NUMERIC_BIT.""mod"": modulus Truncated"
          severity WARNING;
    end if;
    return RESIZE(XREM, L'LENGTH);
  end "mod";

  -- Id: A.36
  function "mod" (L: NATURAL; R: UNSIGNED) return UNSIGNED is
    constant L_LENGTH: NATURAL := MAX(UNSIGNED_NUM_BITS(L), R'LENGTH);
    variable XL, XREM: UNSIGNED(L_LENGTH-1 downto 0);
  begin
    if (R'LENGTH < 1) then return NAU;
    end if;
    XL := TO_UNSIGNED(L, L_LENGTH);
    XREM := RESIZE((XL mod R), XREM'LENGTH);
    if L_LENGTH > R'LENGTH and XREM(L_LENGTH-1 downto R'LENGTH)
        /= (L_LENGTH-1 downto R'LENGTH => '0')
        then
      assert NO_WARNING report "NUMERIC_BIT.""mod"": modulus Truncated"
          severity WARNING;
    end if;
    return RESIZE(XREM, R'LENGTH);
  end "mod";

  -- Id: A.37
  function "mod" (L: SIGNED; R: INTEGER) return SIGNED is
    constant R_LENGTH: NATURAL := MAX(L'LENGTH, SIGNED_NUM_BITS(R));
    variable XR, XREM: SIGNED(R_LENGTH-1 downto 0);
  begin
    if (L'LENGTH < 1) then return NAS;
    end if;
    XR := TO_SIGNED(R, R_LENGTH);
    XREM := RESIZE((L mod XR), XREM'LENGTH);
    if R_LENGTH > L'LENGTH and XREM(R_LENGTH-1 downto L'LENGTH)
        /= (R_LENGTH-1 downto L'LENGTH => XREM(L'LENGTH-1))
        then
      assert NO_WARNING report "NUMERIC_BIT.""mod"": modulus Truncated"
          severity WARNING;
    end if;
    return RESIZE(XREM, L'LENGTH);
  end "mod";

  -- Id: A.38
  function "mod" (L: INTEGER; R: SIGNED) return SIGNED is
    constant L_LENGTH: NATURAL := MAX(SIGNED_NUM_BITS(L), R'LENGTH);
    variable XL, XREM: SIGNED(L_LENGTH-1 downto 0);
  begin
    if (R'LENGTH < 1) then return NAS;
    end if;
    XL := TO_SIGNED(L, L_LENGTH);
    XREM := RESIZE((XL mod R), XREM'LENGTH);
    if L_LENGTH > R'LENGTH and XREM(L_LENGTH-1 downto R'LENGTH)
        /= (L_LENGTH-1 downto R'LENGTH => XREM(R'LENGTH-1))
        then
      assert NO_WARNING report "NUMERIC_BIT.""mod"": modulus Truncated"
          severity WARNING;
    end if;
    return RESIZE(XREM, R'LENGTH);
  end "mod";

  --============================================================================

  -- Id: C.1
  function ">" (L, R: UNSIGNED) return BOOLEAN is
    variable SIZE: NATURAL := MAX(L'LENGTH, R'LENGTH);
  begin
    if ((L'LENGTH < 1) or (R'LENGTH < 1)) then
      assert NO_WARNING
          report "NUMERIC_BIT."">"": null argument detected, returning FALSE"
          severity WARNING;
      return FALSE;
    end if;
    return not UNSIGNED_LESS_OR_EQUAL(RESIZE(L, SIZE), RESIZE(R, SIZE));
  end ">";

  -- Id: C.2
  function ">" (L, R: SIGNED) return BOOLEAN is
    variable SIZE: NATURAL := MAX(L'LENGTH, R'LENGTH);
  begin
    if ((L'LENGTH < 1) or (R'LENGTH < 1)) then
      assert NO_WARNING
          report "NUMERIC_BIT."">"": null argument detected, returning FALSE"
          severity WARNING;
      return FALSE;
    end if;
    return not SIGNED_LESS_OR_EQUAL(RESIZE(L, SIZE), RESIZE(R, SIZE));
  end ">";

  -- Id: C.3
  function ">" (L: NATURAL; R: UNSIGNED) return BOOLEAN is
  begin
    if (R'LENGTH < 1) then
      assert NO_WARNING
          report "NUMERIC_BIT."">"": null argument detected, returning FALSE"
          severity WARNING;
      return FALSE;
    end if;
    if UNSIGNED_NUM_BITS(L) > R'LENGTH then return TRUE;
    end if;
    return not UNSIGNED_LESS_OR_EQUAL(TO_UNSIGNED(L, R'LENGTH), R);
  end ">";

  -- Id: C.4
  function ">" (L: INTEGER; R: SIGNED) return BOOLEAN is
  begin
    if (R'LENGTH < 1) then
      assert NO_WARNING
          report "NUMERIC_BIT."">"": null argument detected, returning FALSE"
          severity WARNING;
      return FALSE;
    end if;
    if SIGNED_NUM_BITS(L) > R'LENGTH then return L > 0;
    end if;
    return not SIGNED_LESS_OR_EQUAL(TO_SIGNED(L, R'LENGTH), R);
  end ">";

  -- Id: C.5
  function ">" (L: UNSIGNED; R: NATURAL) return BOOLEAN is
  begin
    if (L'LENGTH < 1) then
      assert NO_WARNING
          report "NUMERIC_BIT."">"": null argument detected, returning FALSE"
          severity WARNING;
      return FALSE;
    end if;
    if UNSIGNED_NUM_BITS(R) > L'LENGTH then return FALSE;
    end if;
    return not UNSIGNED_LESS_OR_EQUAL(L, TO_UNSIGNED(R, L'LENGTH));
  end ">";

  -- Id: C.6
  function ">" (L: SIGNED; R: INTEGER) return BOOLEAN is
  begin
    if (L'LENGTH < 1) then
      assert NO_WARNING
          report "NUMERIC_BIT."">"": null argument detected, returning FALSE"
          severity WARNING;
      return FALSE;
    end if;
    if SIGNED_NUM_BITS(R) > L'LENGTH then return 0 > R;
    end if;
    return not SIGNED_LESS_OR_EQUAL(L, TO_SIGNED(R, L'LENGTH));
  end ">";

  --============================================================================

  -- Id: C.7
  function "<" (L, R: UNSIGNED) return BOOLEAN is
    variable SIZE: NATURAL := MAX(L'LENGTH, R'LENGTH);
  begin
    if ((L'LENGTH < 1) or (R'LENGTH < 1)) then
      assert NO_WARNING
          report "NUMERIC_BIT.""<"": null argument detected, returning FALSE"
          severity WARNING;
      return FALSE;
    end if;
    return UNSIGNED_LESS(RESIZE(L, SIZE), RESIZE(R, SIZE));
  end "<";

  -- Id: C.8
  function "<" (L, R: SIGNED) return BOOLEAN is
    variable SIZE: NATURAL := MAX(L'LENGTH, R'LENGTH);
  begin
    if ((L'LENGTH < 1) or (R'LENGTH < 1)) then
      assert NO_WARNING
          report "NUMERIC_BIT.""<"": null argument detected, returning FALSE"
          severity WARNING;
      return FALSE;
    end if;
    return SIGNED_LESS(RESIZE(L, SIZE), RESIZE(R, SIZE));
  end "<";

  -- Id: C.9
  function "<" (L: NATURAL; R: UNSIGNED) return BOOLEAN is
  begin
    if (R'LENGTH < 1) then
      assert NO_WARNING
          report "NUMERIC_BIT.""<"": null argument detected, returning FALSE"
          severity WARNING;
      return FALSE;
    end if;
    if UNSIGNED_NUM_BITS(L) > R'LENGTH then return L < 0;
    end if;
    return UNSIGNED_LESS(TO_UNSIGNED(L, R'LENGTH), R);
  end "<";

  -- Id: C.10
  function "<" (L: INTEGER; R: SIGNED) return BOOLEAN is
  begin
    if (R'LENGTH < 1) then
      assert NO_WARNING
          report "NUMERIC_BIT.""<"": null argument detected, returning FALSE"
          severity WARNING;
      return FALSE;
    end if;
    if SIGNED_NUM_BITS(L) > R'LENGTH then return L < 0;
    end if;
    return SIGNED_LESS(TO_SIGNED(L, R'LENGTH), R);
  end "<";

  -- Id: C.11
  function "<" (L: UNSIGNED; R: NATURAL) return BOOLEAN is
  begin
    if (L'LENGTH < 1) then
      assert NO_WARNING
          report "NUMERIC_BIT.""<"": null argument detected, returning FALSE"
          severity WARNING;
      return FALSE;
    end if;
    if UNSIGNED_NUM_BITS(R) > L'LENGTH then return 0 < R;
    end if;
    return UNSIGNED_LESS(L, TO_UNSIGNED(R, L'LENGTH));
  end "<";

  -- Id: C.12
  function "<" (L: SIGNED; R: INTEGER) return BOOLEAN is
  begin
    if (L'LENGTH < 1) then
      assert NO_WARNING
          report "NUMERIC_BIT.""<"": null argument detected, returning FALSE"
          severity WARNING;
      return FALSE;
    end if;
    if SIGNED_NUM_BITS(R) > L'LENGTH then return 0 < R;
    end if;
    return SIGNED_LESS(L, TO_SIGNED(R, L'LENGTH));
  end "<";

  --============================================================================

  -- Id: C.13
  function "<=" (L, R: UNSIGNED) return BOOLEAN is
    variable SIZE: NATURAL := MAX(L'LENGTH, R'LENGTH);
  begin
    if ((L'LENGTH < 1) or (R'LENGTH < 1)) then
      assert NO_WARNING
          report "NUMERIC_BIT.""<="": null argument detected, returning FALSE"
          severity WARNING;
      return FALSE;
    end if;
    return UNSIGNED_LESS_OR_EQUAL(RESIZE(L, SIZE), RESIZE(R, SIZE));
  end "<=";

  -- Id: C.14
  function "<=" (L, R: SIGNED) return BOOLEAN is
    variable SIZE: NATURAL := MAX(L'LENGTH, R'LENGTH);
  begin
    if ((L'LENGTH < 1) or (R'LENGTH < 1)) then
      assert NO_WARNING
          report "NUMERIC_BIT.""<="": null argument detected, returning FALSE"
          severity WARNING;
      return FALSE;
    end if;
    return SIGNED_LESS_OR_EQUAL(RESIZE(L, SIZE), RESIZE(R, SIZE));
  end "<=";

  -- Id: C.15
  function "<=" (L: NATURAL; R: UNSIGNED) return BOOLEAN is
  begin
    if (R'LENGTH < 1) then
      assert NO_WARNING
          report "NUMERIC_BIT.""<="": null argument detected, returning FALSE"
          severity WARNING;
      return FALSE;
    end if;
    if UNSIGNED_NUM_BITS(L) > R'LENGTH then return L < 0;
    end if;
    return UNSIGNED_LESS_OR_EQUAL(TO_UNSIGNED(L, R'LENGTH), R);
  end "<=";

  -- Id: C.16
  function "<=" (L: INTEGER; R: SIGNED) return BOOLEAN is
  begin
    if (R'LENGTH < 1) then
      assert NO_WARNING
          report "NUMERIC_BIT.""<="": null argument detected, returning FALSE"
          severity WARNING;
      return FALSE;
    end if;
    if SIGNED_NUM_BITS(L) > R'LENGTH then return L < 0;
    end if;
    return SIGNED_LESS_OR_EQUAL(TO_SIGNED(L, R'LENGTH), R);
  end "<=";

  -- Id: C.17
  function "<=" (L: UNSIGNED; R: NATURAL) return BOOLEAN is
  begin
    if (L'LENGTH < 1) then
      assert NO_WARNING
          report "NUMERIC_BIT.""<="": null argument detected, returning FALSE"
          severity WARNING;
      return FALSE;
    end if;
    if UNSIGNED_NUM_BITS(R) > L'LENGTH then return 0 < R;
    end if;
    return UNSIGNED_LESS_OR_EQUAL(L, TO_UNSIGNED(R, L'LENGTH));
  end "<=";

  -- Id: C.18
  function "<=" (L: SIGNED; R: INTEGER) return BOOLEAN is
  begin
    if (L'LENGTH < 1) then
      assert NO_WARNING
          report "NUMERIC_BIT.""<="": null argument detected, returning FALSE"
          severity WARNING;
      return FALSE;
    end if;
    if SIGNED_NUM_BITS(R) > L'LENGTH then return 0 < R;
    end if;
    return SIGNED_LESS_OR_EQUAL(L, TO_SIGNED(R, L'LENGTH));
  end "<=";

  --============================================================================

  -- Id: C.19
  function ">=" (L, R: UNSIGNED) return BOOLEAN is
    variable SIZE: NATURAL := MAX(L'LENGTH, R'LENGTH);
  begin
    if ((L'LENGTH < 1) or (R'LENGTH < 1)) then
      assert NO_WARNING
          report "NUMERIC_BIT."">="": null argument detected, returning FALSE"
          severity WARNING;
      return FALSE;
    end if;
    return not UNSIGNED_LESS(RESIZE(L, SIZE), RESIZE(R, SIZE));
  end ">=";

  -- Id: C.20
  function ">=" (L, R: SIGNED) return BOOLEAN is
    variable SIZE: NATURAL := MAX(L'LENGTH, R'LENGTH);
  begin
    if ((L'LENGTH < 1) or (R'LENGTH < 1)) then
      assert NO_WARNING
          report "NUMERIC_BIT."">="": null argument detected, returning FALSE"
          severity WARNING;
      return FALSE;
    end if;
    return not SIGNED_LESS(RESIZE(L, SIZE), RESIZE(R, SIZE));
  end ">=";

  -- Id: C.21
  function ">=" (L: NATURAL; R: UNSIGNED) return BOOLEAN is
  begin
    if (R'LENGTH < 1) then
      assert NO_WARNING
          report "NUMERIC_BIT."">="": null argument detected, returning FALSE"
          severity WARNING;
      return FALSE;
    end if;
    if UNSIGNED_NUM_BITS(L) > R'LENGTH then return L > 0;
    end if;
    return not UNSIGNED_LESS(TO_UNSIGNED(L, R'LENGTH), R);
  end ">=";

  -- Id: C.22
  function ">=" (L: INTEGER; R: SIGNED) return BOOLEAN is
  begin
    if (R'LENGTH < 1) then
      assert NO_WARNING
          report "NUMERIC_BIT."">="": null argument detected, returning FALSE"
          severity WARNING;
      return FALSE;
    end if;
    if SIGNED_NUM_BITS(L) > R'LENGTH then return L > 0;
    end if;
    return not SIGNED_LESS(TO_SIGNED(L, R'LENGTH), R);
  end ">=";

  -- Id: C.23
  function ">=" (L: UNSIGNED; R: NATURAL) return BOOLEAN is
  begin
    if (L'LENGTH < 1) then
      assert NO_WARNING
          report "NUMERIC_BIT."">="": null argument detected, returning FALSE"
          severity WARNING;
      return FALSE;
    end if;
    if UNSIGNED_NUM_BITS(R) > L'LENGTH then return 0 > R;
    end if;
    return not UNSIGNED_LESS(L, TO_UNSIGNED(R, L'LENGTH));
  end ">=";

  -- Id: C.24
  function ">=" (L: SIGNED; R: INTEGER) return BOOLEAN is
  begin
    if (L'LENGTH < 1) then
      assert NO_WARNING
          report "NUMERIC_BIT."">="": null argument detected, returning FALSE"
          severity WARNING;
      return FALSE;
    end if;
    if SIGNED_NUM_BITS(R) > L'LENGTH then return 0 > R;
    end if;
    return not SIGNED_LESS(L, TO_SIGNED(R, L'LENGTH));
  end ">=";

  --============================================================================

  -- Id: C.25
  function "=" (L, R: UNSIGNED) return BOOLEAN is
    variable SIZE: NATURAL := MAX(L'LENGTH, R'LENGTH);
  begin
    if ((L'LENGTH < 1) or (R'LENGTH < 1)) then
      assert NO_WARNING
          report "NUMERIC_BIT.""="": null argument detected, returning FALSE"
          severity WARNING;
      return FALSE;
    end if;
    return UNSIGNED_EQUAL(RESIZE(L, SIZE), RESIZE(R, SIZE));
  end "=";

  -- Id: C.26
  function "=" (L, R: SIGNED) return BOOLEAN is
    variable SIZE: NATURAL := MAX(L'LENGTH, R'LENGTH);
  begin
    if ((L'LENGTH < 1) or (R'LENGTH < 1)) then
      assert NO_WARNING
          report "NUMERIC_BIT.""="": null argument detected, returning FALSE"
          severity WARNING;
      return FALSE;
    end if;
    return SIGNED_EQUAL(RESIZE(L, SIZE), RESIZE(R, SIZE));
  end "=";

  -- Id: C.27
  function "=" (L: NATURAL; R: UNSIGNED) return BOOLEAN is
  begin
    if (R'LENGTH < 1) then
      assert NO_WARNING
          report "NUMERIC_BIT.""="": null argument detected, returning FALSE"
          severity WARNING;
      return FALSE;
    end if;
    if UNSIGNED_NUM_BITS(L) > R'LENGTH then return FALSE;
    end if;
    return UNSIGNED_EQUAL(TO_UNSIGNED(L, R'LENGTH), R);
  end "=";

  -- Id: C.28
  function "=" (L: INTEGER; R: SIGNED) return BOOLEAN is
  begin
    if (R'LENGTH < 1) then
      assert NO_WARNING
          report "NUMERIC_BIT.""="": null argument detected, returning FALSE"
          severity WARNING;
      return FALSE;
    end if;
    if SIGNED_NUM_BITS(L) > R'LENGTH then return FALSE;
    end if;
    return SIGNED_EQUAL(TO_SIGNED(L, R'LENGTH), R);
  end "=";

  -- Id: C.29
  function "=" (L: UNSIGNED; R: NATURAL) return BOOLEAN is
  begin
    if (L'LENGTH < 1) then
      assert NO_WARNING
          report "NUMERIC_BIT.""="": null argument detected, returning FALSE"
          severity WARNING;
      return FALSE;
    end if;
    if UNSIGNED_NUM_BITS(R) > L'LENGTH then return FALSE;
    end if;
    return UNSIGNED_EQUAL(L, TO_UNSIGNED(R, L'LENGTH));
  end "=";

  -- Id: C.30
  function "=" (L: SIGNED; R: INTEGER) return BOOLEAN is
  begin
    if (L'LENGTH < 1) then
      assert NO_WARNING
          report "NUMERIC_BIT.""="": null argument detected, returning FALSE"
          severity WARNING;
      return FALSE;
    end if;
    if SIGNED_NUM_BITS(R) > L'LENGTH then return FALSE;
    end if;
    return SIGNED_EQUAL(L, TO_SIGNED(R, L'LENGTH));
  end "=";

  --============================================================================

  -- Id: C.31
  function "/=" (L, R: UNSIGNED) return BOOLEAN is
    variable SIZE: NATURAL := MAX(L'LENGTH, R'LENGTH);
  begin
    if ((L'LENGTH < 1) or (R'LENGTH < 1)) then
      assert NO_WARNING
          report "NUMERIC_BIT.""/="": null argument detected, returning TRUE"
          severity WARNING;
      return TRUE;
    end if;
    return not(UNSIGNED_EQUAL(RESIZE(L, SIZE), RESIZE(R, SIZE)));
  end "/=";

  -- Id: C.32
  function "/=" (L, R: SIGNED) return BOOLEAN is
    variable SIZE: NATURAL := MAX(L'LENGTH, R'LENGTH);
  begin
    if ((L'LENGTH < 1) or (R'LENGTH < 1)) then
      assert NO_WARNING
          report "NUMERIC_BIT.""/="": null argument detected, returning TRUE"
          severity WARNING;
      return TRUE;
    end if;
    return not(SIGNED_EQUAL(RESIZE(L, SIZE), RESIZE(R, SIZE)));
  end "/=";

  -- Id: C.33
  function "/=" (L: NATURAL; R: UNSIGNED) return BOOLEAN is
  begin
    if (R'LENGTH < 1) then
      assert NO_WARNING
          report "NUMERIC_BIT.""/="": null argument detected, returning TRUE"
          severity WARNING;
      return TRUE;
    end if;
    if UNSIGNED_NUM_BITS(L) > R'LENGTH then return TRUE;
    end if;
    return not(UNSIGNED_EQUAL(TO_UNSIGNED(L, R'LENGTH), R));
  end "/=";

  -- Id: C.34
  function "/=" (L: INTEGER; R: SIGNED) return BOOLEAN is
  begin
    if (R'LENGTH < 1) then
      assert NO_WARNING
          report "NUMERIC_BIT.""/="": null argument detected, returning TRUE"
          severity WARNING;
      return TRUE;
    end if;
    if SIGNED_NUM_BITS(L) > R'LENGTH then return TRUE;
    end if;
    return not(SIGNED_EQUAL(TO_SIGNED(L, R'LENGTH), R));
  end "/=";

  -- Id: C.35
  function "/=" (L: UNSIGNED; R: NATURAL) return BOOLEAN is
  begin
    if (L'LENGTH < 1) then
      assert NO_WARNING
          report "NUMERIC_BIT.""/="": null argument detected, returning TRUE"
          severity WARNING;
      return TRUE;
    end if;
    if UNSIGNED_NUM_BITS(R) > L'LENGTH then return TRUE;
    end if;
    return not(UNSIGNED_EQUAL(L, TO_UNSIGNED(R, L'LENGTH)));
  end "/=";

  -- Id: C.36
  function "/=" (L: SIGNED; R: INTEGER) return BOOLEAN is
  begin
    if (L'LENGTH < 1) then
      assert NO_WARNING
          report "NUMERIC_BIT.""/="": null argument detected, returning TRUE"
          severity WARNING;
      return TRUE;
    end if;
    if SIGNED_NUM_BITS(R) > L'LENGTH then return TRUE;
    end if;
    return not(SIGNED_EQUAL(L, TO_SIGNED(R, L'LENGTH)));
  end "/=";

  --============================================================================

  -- Id: S.1
  function SHIFT_LEFT (ARG: UNSIGNED; COUNT: NATURAL) return UNSIGNED is
  begin
    if (ARG'LENGTH < 1) then return NAU;
    end if;
    return UNSIGNED(XSLL(BIT_VECTOR(ARG), COUNT));
  end SHIFT_LEFT;

  -- Id: S.2
  function SHIFT_RIGHT (ARG: UNSIGNED; COUNT: NATURAL) return UNSIGNED is
  begin
    if (ARG'LENGTH < 1) then return NAU;
    end if;
    return UNSIGNED(XSRL(BIT_VECTOR(ARG), COUNT));
  end SHIFT_RIGHT;

  -- Id: S.3
  function SHIFT_LEFT (ARG: SIGNED; COUNT: NATURAL) return SIGNED is
  begin
    if (ARG'LENGTH < 1) then return NAS;
    end if;
    return SIGNED(XSLL(BIT_VECTOR(ARG), COUNT));
  end SHIFT_LEFT;

  -- Id: S.4
  function SHIFT_RIGHT (ARG: SIGNED; COUNT: NATURAL) return SIGNED is
  begin
    if (ARG'LENGTH < 1) then return NAS;
    end if;
    return SIGNED(XSRA(BIT_VECTOR(ARG), COUNT));
  end SHIFT_RIGHT;

  --============================================================================

  -- Id: S.5
  function ROTATE_LEFT (ARG: UNSIGNED; COUNT: NATURAL) return UNSIGNED is
  begin
    if (ARG'LENGTH < 1) then return NAU;
    end if;
    return UNSIGNED(XROL(BIT_VECTOR(ARG), COUNT));
  end ROTATE_LEFT;

  -- Id: S.6
  function ROTATE_RIGHT (ARG: UNSIGNED; COUNT: NATURAL) return UNSIGNED is
  begin
    if (ARG'LENGTH < 1) then return NAU;
    end if;
    return UNSIGNED(XROR(BIT_VECTOR(ARG), COUNT));
  end ROTATE_RIGHT;

  -- Id: S.7
  function ROTATE_LEFT (ARG: SIGNED; COUNT: NATURAL) return SIGNED is
  begin
    if (ARG'LENGTH < 1) then return NAS;
    end if;
    return SIGNED(XROL(BIT_VECTOR(ARG), COUNT));
  end ROTATE_LEFT;

  -- Id: S.8
  function ROTATE_RIGHT (ARG: SIGNED; COUNT: NATURAL) return SIGNED is
  begin
    if (ARG'LENGTH < 1) then return NAS;
    end if;
    return SIGNED(XROR(BIT_VECTOR(ARG), COUNT));
  end ROTATE_RIGHT;

  --============================================================================

--START-V93
  ------------------------------------------------------------------------------
  -- Note : Function S.9 is not compatible with VHDL 1076-1987. Comment
  -- out the function (declaration and body) for VHDL 1076-1987 compatibility.
  ------------------------------------------------------------------------------
  -- Id: S.9
  function "sll" (ARG: UNSIGNED; COUNT: INTEGER) return UNSIGNED is
  begin
    if (COUNT >= 0) then
      return SHIFT_LEFT(ARG, COUNT);
    else
      return SHIFT_RIGHT(ARG, -COUNT);
    end if;
  end "sll";

  ------------------------------------------------------------------------------
  -- Note : Function S.10 is not compatible with VHDL 1076-1987. Comment
  -- out the function (declaration and body) for VHDL 1076-1987 compatibility.
  ------------------------------------------------------------------------------
  -- Id: S.10
  function "sll" (ARG: SIGNED; COUNT: INTEGER) return SIGNED is
  begin
    if (COUNT >= 0) then
      return SHIFT_LEFT(ARG, COUNT);
    else
      return SIGNED(SHIFT_RIGHT(UNSIGNED(ARG), -COUNT));
    end if;
  end "sll";

  ------------------------------------------------------------------------------
  -- Note : Function S.11 is not compatible with VHDL 1076-1987. Comment
  -- out the function (declaration and body) for VHDL 1076-1987 compatibility.
  ------------------------------------------------------------------------------
  -- Id: S.11
  function "srl" (ARG: UNSIGNED; COUNT: INTEGER) return UNSIGNED is
  begin
    if (COUNT >= 0) then
      return SHIFT_RIGHT(ARG, COUNT);
    else
      return SHIFT_LEFT(ARG, -COUNT);
    end if;
  end "srl";

  ------------------------------------------------------------------------------
  -- Note : Function S.12 is not compatible with VHDL 1076-1987. Comment
  -- out the function (declaration and body) for VHDL 1076-1987 compatibility.
  ------------------------------------------------------------------------------
  -- Id: S.12
  function "srl" (ARG: SIGNED; COUNT: INTEGER) return SIGNED is
  begin
    if (COUNT >= 0) then
      return SIGNED(SHIFT_RIGHT(UNSIGNED(ARG), COUNT));
    else
      return SHIFT_LEFT(ARG, -COUNT);
    end if;
  end "srl";

  ------------------------------------------------------------------------------
  -- Note : Function S.13 is not compatible with VHDL 1076-1987. Comment
  -- out the function (declaration and body) for VHDL 1076-1987 compatibility.
  ------------------------------------------------------------------------------
  -- Id: S.13
  function "rol" (ARG: UNSIGNED; COUNT: INTEGER) return UNSIGNED is
  begin
    if (COUNT >= 0) then
      return ROTATE_LEFT(ARG, COUNT);
    else
      return ROTATE_RIGHT(ARG, -COUNT);
    end if;
  end "rol";

  ------------------------------------------------------------------------------
  -- Note : Function S.14 is not compatible with VHDL 1076-1987. Comment
  -- out the function (declaration and body) for VHDL 1076-1987 compatibility.
  ------------------------------------------------------------------------------
  -- Id: S.14
  function "rol" (ARG: SIGNED; COUNT: INTEGER) return SIGNED is
  begin
    if (COUNT >= 0) then
      return ROTATE_LEFT(ARG, COUNT);
    else
      return ROTATE_RIGHT(ARG, -COUNT);
    end if;
  end "rol";

  ------------------------------------------------------------------------------
  -- Note : Function S.15 is not compatible with VHDL 1076-1987. Comment
  -- out the function (declaration and body) for VHDL 1076-1987 compatibility.
  ------------------------------------------------------------------------------
  -- Id: S.15
  function "ror" (ARG: UNSIGNED; COUNT: INTEGER) return UNSIGNED is
  begin
    if (COUNT >= 0) then
      return ROTATE_RIGHT(ARG, COUNT);
    else
      return ROTATE_LEFT(ARG, -COUNT);
    end if;
  end "ror";

  ------------------------------------------------------------------------------
  -- Note : Function S.16 is not compatible with VHDL 1076-1987. Comment
  -- out the function (declaration and body) for VHDL 1076-1987 compatibility.
  ------------------------------------------------------------------------------
  -- Id: S.16
  function "ror" (ARG: SIGNED; COUNT: INTEGER) return SIGNED is
  begin
    if (COUNT >= 0) then
      return ROTATE_RIGHT(ARG, COUNT);
    else
      return ROTATE_LEFT(ARG, -COUNT);
    end if;
  end "ror";

--END-V93
  --============================================================================

  -- Id: D.1
  function TO_INTEGER (ARG: UNSIGNED) return NATURAL is
    constant ARG_LEFT: INTEGER := ARG'LENGTH-1;
    alias XARG: UNSIGNED(ARG_LEFT downto 0) is ARG;
    variable RESULT: NATURAL := 0;
  begin
    if (ARG'LENGTH < 1) then
      assert NO_WARNING
          report "NUMERIC_BIT.TO_INTEGER: null detected, returning 0"
          severity WARNING;
      return 0;
    end if;
    for I in XARG'RANGE loop
      RESULT := RESULT+RESULT;
      if XARG(I) = '1' then
        RESULT := RESULT + 1;
      end if;
    end loop;
    return RESULT;
  end TO_INTEGER;

  -- Id: D.2
  function TO_INTEGER (ARG: SIGNED) return INTEGER is
  begin
    if (ARG'LENGTH < 1) then
      assert NO_WARNING
          report "NUMERIC_BIT.TO_INTEGER: null detected, returning 0"
          severity WARNING;
      return 0;
    end if;
    if ARG(ARG'LEFT) = '0' then
      return TO_INTEGER(UNSIGNED(ARG));
    else
      return (- (TO_INTEGER(UNSIGNED(- (ARG + 1)))) -1);
    end if;
  end TO_INTEGER;

  -- Id: D.3
  function TO_UNSIGNED (ARG, SIZE: NATURAL) return UNSIGNED is
    variable RESULT: UNSIGNED(SIZE-1 downto 0);
    variable I_VAL: NATURAL := ARG;
  begin
    if (SIZE < 1) then return NAU;
    end if;
    for I in 0 to RESULT'LEFT loop
      if (I_VAL mod 2) = 0 then
        RESULT(I) := '0';
      else RESULT(I) := '1';
      end if;
      I_VAL := I_VAL/2;
    end loop;
    if not(I_VAL =0) then
      assert NO_WARNING
          report "NUMERIC_BIT.TO_UNSIGNED: vector truncated"
          severity WARNING;
    end if;
    return RESULT;
  end TO_UNSIGNED;

  -- Id: D.4
  function TO_SIGNED (ARG: INTEGER;
    SIZE: NATURAL) return SIGNED is
        variable RESULT: SIGNED(SIZE-1 downto 0);
    variable B_VAL: BIT := '0';
    variable I_VAL: INTEGER := ARG;
  begin
    if (SIZE < 1) then return NAS;
    end if;
    if (ARG < 0) then
      B_VAL := '1';
      I_VAL := -(ARG+1);
    end if;
    for I in 0 to RESULT'LEFT loop
      if (I_VAL mod 2) = 0 then
        RESULT(I) := B_VAL;
      else
        RESULT(I) := not B_VAL;
      end if;
      I_VAL := I_VAL/2;
    end loop;
    if ((I_VAL/=0) or (B_VAL/=RESULT(RESULT'LEFT))) then
      assert NO_WARNING
          report "NUMERIC_BIT.TO_SIGNED: vector truncated"
          severity WARNING;
    end if;
    return RESULT;
  end TO_SIGNED;

  --============================================================================

  -- Id: R.1
  function RESIZE (ARG: SIGNED; NEW_SIZE: NATURAL) return SIGNED is
    alias INVEC: SIGNED(ARG'LENGTH-1 downto 0) is ARG;
    variable RESULT: SIGNED(NEW_SIZE-1 downto 0) := (others => '0');
    constant BOUND: INTEGER := MIN(ARG'LENGTH, RESULT'LENGTH)-2;
  begin
    if (NEW_SIZE < 1) then return NAS;
    end if;
    if (ARG'LENGTH = 0) then return RESULT;
    end if;
    RESULT := (others => ARG(ARG'LEFT));
    if BOUND >= 0 then
      RESULT(BOUND downto 0) := INVEC(BOUND downto 0);
    end if;
    return RESULT;
  end RESIZE;

  -- Id: R.2
  function RESIZE (ARG: UNSIGNED; NEW_SIZE: NATURAL) return UNSIGNED is
    constant ARG_LEFT: INTEGER := ARG'LENGTH-1;
    alias XARG: UNSIGNED(ARG_LEFT downto 0) is ARG;
    variable RESULT: UNSIGNED(NEW_SIZE-1 downto 0) := (others => '0');
  begin
    if (NEW_SIZE < 1) then return NAU;
    end if;
    if XARG'LENGTH =0 then return RESULT;
    end if;
    if (RESULT'LENGTH < ARG'LENGTH) then
      RESULT(RESULT'LEFT downto 0) := XARG(RESULT'LEFT downto 0);
    else
      RESULT(RESULT'LEFT downto XARG'LEFT+1) := (others => '0');
      RESULT(XARG'LEFT downto 0) := XARG;
    end if;
    return RESULT;
  end RESIZE;

  --============================================================================

  -- Id: L.1
  function "not" (L: UNSIGNED) return UNSIGNED is
    variable RESULT: UNSIGNED(L'LENGTH-1 downto 0);
  begin
    RESULT := UNSIGNED(not(BIT_VECTOR(L)));
    return RESULT;
  end "not";

  -- Id: L.2
  function "and" (L, R: UNSIGNED) return UNSIGNED is
    variable RESULT: UNSIGNED(L'LENGTH-1 downto 0);
  begin
    RESULT := UNSIGNED(BIT_VECTOR(L) and BIT_VECTOR(R));
    return RESULT;
  end "and";

  -- Id: L.3
  function "or" (L, R: UNSIGNED) return UNSIGNED is
    variable RESULT: UNSIGNED(L'LENGTH-1 downto 0);
  begin
    RESULT := UNSIGNED(BIT_VECTOR(L) or BIT_VECTOR(R));
    return RESULT;
  end "or";

  -- Id: L.4
  function "nand" (L, R: UNSIGNED) return UNSIGNED is
    variable RESULT: UNSIGNED(L'LENGTH-1 downto 0);
  begin
    RESULT := UNSIGNED(BIT_VECTOR(L) nand BIT_VECTOR(R));
    return RESULT;
  end "nand";

  -- Id: L.5
  function "nor" (L, R: UNSIGNED) return UNSIGNED is
    variable RESULT: UNSIGNED(L'LENGTH-1 downto 0);
  begin
    RESULT := UNSIGNED(BIT_VECTOR(L) nor BIT_VECTOR(R));
    return RESULT;
  end "nor";

  -- Id: L.6
  function "xor" (L, R: UNSIGNED) return UNSIGNED is
    variable RESULT: UNSIGNED(L'LENGTH-1 downto 0);
  begin
    RESULT := UNSIGNED(BIT_VECTOR(L) xor BIT_VECTOR(R));
    return RESULT;
  end "xor";

--START-V93
  ------------------------------------------------------------------------------
  -- Note : Function L.7 is not compatible with VHDL 1076-1987. Comment
  -- out the function (declaration and body) for VHDL 1076-1987 compatibility.
  ------------------------------------------------------------------------------
  -- Id: L.7
  function "xnor" (L, R: UNSIGNED) return UNSIGNED is
    variable RESULT: UNSIGNED(L'LENGTH-1 downto 0);
  begin
    RESULT := UNSIGNED(BIT_VECTOR(L) xnor BIT_VECTOR(R));
    return RESULT;
  end "xnor";
--END-V93

  -- Id: L.8
  function "not" (L: SIGNED) return SIGNED is
    variable RESULT: SIGNED(L'LENGTH-1 downto 0);
  begin
    RESULT := SIGNED(not(BIT_VECTOR(L)));
    return RESULT;
  end "not";

  -- Id: L.9
  function "and" (L, R: SIGNED) return SIGNED is
    variable RESULT: SIGNED(L'LENGTH-1 downto 0);
  begin
    RESULT := SIGNED(BIT_VECTOR(L) and BIT_VECTOR(R));
    return RESULT;
  end "and";

  -- Id: L.10
  function "or" (L, R: SIGNED) return SIGNED is
    variable RESULT: SIGNED(L'LENGTH-1 downto 0);
  begin
    RESULT := SIGNED(BIT_VECTOR(L) or BIT_VECTOR(R));
    return RESULT;
  end "or";

  -- Id: L.11
  function "nand" (L, R: SIGNED) return SIGNED is
    variable RESULT: SIGNED(L'LENGTH-1 downto 0);
  begin
    RESULT := SIGNED(BIT_VECTOR(L) nand BIT_VECTOR(R));
    return RESULT;
  end "nand";

  -- Id: L.12
  function "nor" (L, R: SIGNED) return SIGNED is
    variable RESULT: SIGNED(L'LENGTH-1 downto 0);
  begin
    RESULT := SIGNED(BIT_VECTOR(L) nor BIT_VECTOR(R));
    return RESULT;
  end "nor";

  -- Id: L.13
  function "xor" (L, R: SIGNED) return SIGNED is
    variable RESULT: SIGNED(L'LENGTH-1 downto 0);
  begin
    RESULT := SIGNED(BIT_VECTOR(L) xor BIT_VECTOR(R));
    return RESULT;
  end "xor";

--START-V93
  ------------------------------------------------------------------------------
  -- Note : Function L.14 is not compatible with VHDL 1076-1987. Comment
  -- out the function (declaration and body) for VHDL 1076-1987 compatibility.
  ------------------------------------------------------------------------------
  -- Id: L.14
  function "xnor" (L, R: SIGNED) return SIGNED is
    variable RESULT: SIGNED(L'LENGTH-1 downto 0);
  begin
    RESULT := SIGNED(BIT_VECTOR(L) xnor BIT_VECTOR(R));
    return RESULT;
  end "xnor";
--END-V93

  --============================================================================

  -- Id: E.1
  function RISING_EDGE (signal S: BIT) return BOOLEAN is
  begin
    return S'EVENT and S = '1';
  end RISING_EDGE;

  -- Id: E.2
  function FALLING_EDGE (signal S: BIT) return BOOLEAN is
  begin
    return S'EVENT and S = '0';
  end FALLING_EDGE;

  --============================================================================
end NUMERIC_BIT;
