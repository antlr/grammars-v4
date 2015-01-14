-- --------------------------------------------------------------------
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
-- Title      : Standard VHDL Synthesis Package (1076.3, NUMERIC_STD)
--
-- Library    : This package shall be compiled into a library symbolically
--            : named IEEE.
--
-- Developers : IEEE DASC Synthesis Working Group, PAR 1076.3
--
-- Purpose    : This package defines numeric types and arithmetic functions
--            : for use with synthesis tools. Two numeric types are defined:
--            : -- > UNSIGNED: represents UNSIGNED number in vector form
--            : -- > SIGNED: represents a SIGNED number in vector form
--            : The base element type is type STD_LOGIC.
--            : The leftmost bit is treated as the most significant bit.
--            : Signed vectors are represented in two's complement form.
--            : This package contains overloaded arithmetic operators on
--            : the SIGNED and UNSIGNED types. The package also contains
--            : useful type conversions functions.
--            :
--            : If any argument to a function is a null array, a null array is
--            : returned (exceptions, if any, are noted individually).
--
-- Limitation :
--
-- Note       : No declarations or definitions shall be included in,
--            : or excluded from this package. The "package declaration"
--            : defines the types, subtypes and declarations of
--            : NUMERIC_STD. The NUMERIC_STD package body shall be
--            : considered the formal definition of the semantics of
--            : this package. Tool developers may choose to implement
--            : the package body in the most efficient manner available
--            : to them.
--
-- --------------------------------------------------------------------
--   modification history :
-- --------------------------------------------------------------------
--   Version:  2.4
--   Date   :  12 April 1995
-- -----------------------------------------------------------------------------

--==============================================================================
--============================= Package Body ===================================
--==============================================================================

package body NUMERIC_STD is

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

  ------------------------------------------------------------------------

  -- this internal function computes the addition of two UNSIGNED
  -- with input CARRY
  -- * the two arguments are of the same length

  function ADD_UNSIGNED (L, R: UNSIGNED; C: STD_LOGIC) return UNSIGNED is
    constant L_LEFT: INTEGER := L'LENGTH-1;
    alias XL: UNSIGNED(L_LEFT downto 0) is L;
    alias XR: UNSIGNED(L_LEFT downto 0) is R;
    variable RESULT: UNSIGNED(L_LEFT downto 0);
    variable CBIT: STD_LOGIC := C;
  begin
    for I in 0 to L_LEFT loop
      RESULT(I) := CBIT xor XL(I) xor XR(I);
      CBIT := (CBIT and XL(I)) or (CBIT and XR(I)) or (XL(I) and XR(I));
    end loop;
    return RESULT;
  end ADD_UNSIGNED;

  -- this internal function computes the addition of two SIGNED
  -- with input CARRY
  -- * the two arguments are of the same length

  function ADD_SIGNED (L, R: SIGNED; C: STD_LOGIC) return SIGNED is
    constant L_LEFT: INTEGER := L'LENGTH-1;
    alias XL: SIGNED(L_LEFT downto 0) is L;
    alias XR: SIGNED(L_LEFT downto 0) is R;
    variable RESULT: SIGNED(L_LEFT downto 0);
    variable CBIT: STD_LOGIC := C;
  begin
    for I in 0 to L_LEFT loop
      RESULT(I) := CBIT xor XL(I) xor XR(I);
      CBIT := (CBIT and XL(I)) or (CBIT and XR(I)) or (XL(I) and XR(I));
    end loop;
    return RESULT;
  end ADD_SIGNED;

  -----------------------------------------------------------------------------

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

  function XSLL (ARG: STD_LOGIC_VECTOR; COUNT: NATURAL) return STD_LOGIC_VECTOR
      is
    constant ARG_L: INTEGER := ARG'LENGTH-1;
    alias XARG: STD_LOGIC_VECTOR(ARG_L downto 0) is ARG;
    variable RESULT: STD_LOGIC_VECTOR(ARG_L downto 0) := (others => '0');
  begin
    if COUNT <= ARG_L then
      RESULT(ARG_L downto COUNT) := XARG(ARG_L-COUNT downto 0);
    end if;
    return RESULT;
  end XSLL;

  function XSRL (ARG: STD_LOGIC_VECTOR; COUNT: NATURAL) return STD_LOGIC_VECTOR
      is
    constant ARG_L: INTEGER := ARG'LENGTH-1;
    alias XARG: STD_LOGIC_VECTOR(ARG_L downto 0) is ARG;
    variable RESULT: STD_LOGIC_VECTOR(ARG_L downto 0) := (others => '0');
  begin
    if COUNT <= ARG_L then
      RESULT(ARG_L-COUNT downto 0) := XARG(ARG_L downto COUNT);
    end if;
    return RESULT;
  end XSRL;

  function XSRA (ARG: STD_LOGIC_VECTOR; COUNT: NATURAL) return STD_LOGIC_VECTOR
      is
    constant ARG_L: INTEGER := ARG'LENGTH-1;
    alias XARG: STD_LOGIC_VECTOR(ARG_L downto 0) is ARG;
    variable RESULT: STD_LOGIC_VECTOR(ARG_L downto 0);
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

  function XROL (ARG: STD_LOGIC_VECTOR; COUNT: NATURAL) return STD_LOGIC_VECTOR
      is
    constant ARG_L: INTEGER := ARG'LENGTH-1;
    alias XARG: STD_LOGIC_VECTOR(ARG_L downto 0) is ARG;
    variable RESULT: STD_LOGIC_VECTOR(ARG_L downto 0) := XARG;
    variable COUNTM: INTEGER;
  begin
    COUNTM := COUNT mod (ARG_L + 1);
    if COUNTM /= 0 then
      RESULT(ARG_L downto COUNTM) := XARG(ARG_L-COUNTM downto 0);
      RESULT(COUNTM-1 downto 0) := XARG(ARG_L downto ARG_L-COUNTM+1);
    end if;
    return RESULT;
  end XROL;

  function XROR (ARG: STD_LOGIC_VECTOR; COUNT: NATURAL) return STD_LOGIC_VECTOR
      is
    constant ARG_L: INTEGER := ARG'LENGTH-1;
    alias XARG: STD_LOGIC_VECTOR(ARG_L downto 0) is ARG;
    variable RESULT: STD_LOGIC_VECTOR(ARG_L downto 0) := XARG;
    variable COUNTM: INTEGER;
  begin
    COUNTM := COUNT mod (ARG_L + 1);
    if COUNTM /= 0 then
      RESULT(ARG_L-COUNTM downto 0) := XARG(ARG_L downto COUNTM);
      RESULT(ARG_L downto ARG_L-COUNTM+1) := XARG(COUNTM-1 downto 0);
    end if;
    return RESULT;
  end XROR;

  -----------------Local Subprograms - Relational ops---------------------------

  --
  -- General "=" for UNSIGNED vectors, same length
  --
  function UNSIGNED_EQUAL (L, R: UNSIGNED) return BOOLEAN is
  begin
    return STD_LOGIC_VECTOR(L) = STD_LOGIC_VECTOR(R);
  end UNSIGNED_EQUAL;

  --
  -- General "=" for SIGNED vectors, same length
  --
  function SIGNED_EQUAL (L, R: SIGNED) return BOOLEAN is
  begin
    return STD_LOGIC_VECTOR(L) = STD_LOGIC_VECTOR(R);
  end SIGNED_EQUAL;

  --
  -- General "<" for UNSIGNED vectors, same length
  --
  function UNSIGNED_LESS (L, R: UNSIGNED) return BOOLEAN is
  begin
    return STD_LOGIC_VECTOR(L) < STD_LOGIC_VECTOR(R);
  end UNSIGNED_LESS;

  --
  -- General "<" function for SIGNED vectors, same length
  --
  function SIGNED_LESS (L, R: SIGNED) return BOOLEAN is
    variable INTERN_L: SIGNED(0 to L'LENGTH-1);
    variable INTERN_R: SIGNED(0 to R'LENGTH-1);
  begin
    INTERN_L := L;
    INTERN_R := R;
    INTERN_L(0) := not INTERN_L(0);
    INTERN_R(0) := not INTERN_R(0);
    return STD_LOGIC_VECTOR(INTERN_L) < STD_LOGIC_VECTOR(INTERN_R);
  end SIGNED_LESS;

  --
  -- General "<=" function for UNSIGNED vectors, same length
  --
  function UNSIGNED_LESS_OR_EQUAL (L, R: UNSIGNED) return BOOLEAN is
  begin
    return STD_LOGIC_VECTOR(L) <= STD_LOGIC_VECTOR(R);
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
    return STD_LOGIC_VECTOR(INTERN_L) <= STD_LOGIC_VECTOR(INTERN_R);
  end SIGNED_LESS_OR_EQUAL;

  --=========================Exported Functions ==========================

  -- Id: A.1
  function "abs" (ARG: SIGNED) return SIGNED is
    constant ARG_LEFT: INTEGER := ARG'LENGTH-1;
    alias XARG: SIGNED(ARG_LEFT downto 0) is ARG;
    variable RESULT: SIGNED(ARG_LEFT downto 0);
  begin
    if ARG'LENGTH < 1 then return NAS;
    end if;
    RESULT := TO_01(XARG, 'X');
    if (RESULT(RESULT'LEFT)='X') then return RESULT;
    end if;
    if RESULT(RESULT'LEFT) = '1' then
      RESULT := -RESULT;
    end if;
    return RESULT;
  end "abs";

  -- Id: A.2
  function "-" (ARG: SIGNED) return SIGNED is
    constant ARG_LEFT: INTEGER := ARG'LENGTH-1;
    alias XARG: SIGNED(ARG_LEFT downto 0) is ARG;
    variable RESULT, XARG01 : SIGNED(ARG_LEFT downto 0);
    variable CBIT: STD_LOGIC := '1';
  begin
    if ARG'LENGTH < 1 then return NAS;
    end if;
    XARG01 := TO_01(ARG, 'X');
    if (XARG01(XARG01'LEFT)='X') then return XARG01;
    end if;
    for I in 0 to RESULT'LEFT loop
      RESULT(I) := not(XARG01(I)) xor CBIT;
      CBIT := CBIT and not(XARG01(I));
    end loop;
    return RESULT;
  end "-";

  --============================================================================

  -- Id: A.3
  function "+" (L, R: UNSIGNED) return UNSIGNED is
    constant SIZE: NATURAL := MAX(L'LENGTH, R'LENGTH);
    variable L01 : UNSIGNED(SIZE-1 downto 0);
    variable R01 : UNSIGNED(SIZE-1 downto 0);
  begin
    if ((L'LENGTH < 1) or (R'LENGTH < 1)) then return NAU;
    end if;
    L01 := TO_01(RESIZE(L, SIZE), 'X');
    if (L01(L01'LEFT)='X') then return L01;
    end if;
    R01 := TO_01(RESIZE(R, SIZE), 'X');
    if (R01(R01'LEFT)='X') then return R01;
    end if;
    return ADD_UNSIGNED(L01, R01, '0');
  end "+";

  -- Id: A.4
  function "+" (L, R: SIGNED) return SIGNED is
    constant SIZE: NATURAL := MAX(L'LENGTH, R'LENGTH);
    variable L01 : SIGNED(SIZE-1 downto 0);
    variable R01 : SIGNED(SIZE-1 downto 0);
  begin
    if ((L'LENGTH < 1) or (R'LENGTH < 1)) then return NAS;
    end if;
    L01 := TO_01(RESIZE(L, SIZE), 'X');
    if (L01(L01'LEFT)='X') then return L01;
    end if;
    R01 := TO_01(RESIZE(R, SIZE), 'X');
    if (R01(R01'LEFT)='X') then return R01;
    end if;
    return ADD_SIGNED(L01, R01, '0');
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
    constant SIZE: NATURAL := MAX(L'LENGTH, R'LENGTH);
    variable L01 : UNSIGNED(SIZE-1 downto 0);
    variable R01 : UNSIGNED(SIZE-1 downto 0);
  begin
    if ((L'LENGTH < 1) or (R'LENGTH < 1)) then return NAU;
    end if;
    L01 := TO_01(RESIZE(L, SIZE), 'X');
    if (L01(L01'LEFT)='X') then return L01;
    end if;
    R01 := TO_01(RESIZE(R, SIZE), 'X');
    if (R01(R01'LEFT)='X') then return R01;
    end if;
    return ADD_UNSIGNED(L01, not(R01), '1');
  end "-";

  -- Id: A.10
  function "-" (L, R: SIGNED) return SIGNED is
    constant SIZE: NATURAL := MAX(L'LENGTH, R'LENGTH);
    variable L01 : SIGNED(SIZE-1 downto 0);
    variable R01 : SIGNED(SIZE-1 downto 0);
  begin
    if ((L'LENGTH < 1) or (R'LENGTH < 1)) then return NAS;
    end if;
    L01 := TO_01(RESIZE(L, SIZE), 'X');
    if (L01(L01'LEFT)='X') then return L01;
    end if;
    R01 := TO_01(RESIZE(R, SIZE), 'X');
    if (R01(R01'LEFT)='X') then return R01;
    end if;
    return ADD_SIGNED(L01, not(R01), '1');
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
    alias XXL: UNSIGNED(L_LEFT downto 0) is L;
    alias XXR: UNSIGNED(R_LEFT downto 0) is R;
    variable XL: UNSIGNED(L_LEFT downto 0);
    variable XR: UNSIGNED(R_LEFT downto 0);
    variable RESULT: UNSIGNED((L'LENGTH+R'LENGTH-1) downto 0) :=
        (others => '0');
    variable ADVAL: UNSIGNED((L'LENGTH+R'LENGTH-1) downto 0);
  begin
    if ((L'LENGTH < 1) or (R'LENGTH < 1)) then return NAU;
    end if;
    XL := TO_01(XXL, 'X');
    XR := TO_01(XXR, 'X');
    if ((XL(XL'LEFT)='X') or (XR(XR'LEFT)='X')) then
      RESULT := (others => 'X');
      return RESULT;
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
    XL := TO_01(L, 'X');
    XR := TO_01(R, 'X');
    if ((XL(L_LEFT)='X') or (XR(R_LEFT)='X')) then
      RESULT := (others => 'X');
      return RESULT;
    end if;
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
    constant L_LEFT: INTEGER := L'LENGTH-1;
    constant R_LEFT: INTEGER := R'LENGTH-1;
    alias XXL: UNSIGNED(L_LEFT downto 0) is L;
    alias XXR: UNSIGNED(R_LEFT downto 0) is R;
    variable XL: UNSIGNED(L_LEFT downto 0);
    variable XR: UNSIGNED(R_LEFT downto 0);
    variable FQUOT: UNSIGNED(L'LENGTH-1 downto 0);
    variable FREMAIN: UNSIGNED(R'LENGTH-1 downto 0);
  begin
    if ((L'LENGTH < 1) or (R'LENGTH < 1)) then return NAU;
    end if;
    XL := TO_01(XXL, 'X');
    XR := TO_01(XXR, 'X');
    if ((XL(XL'LEFT)='X') or (XR(XR'LEFT)='X')) then
      FQUOT := (others => 'X');
      return FQUOT;
    end if;
    DIVMOD(XL, XR, FQUOT, FREMAIN);
    return FQUOT;
  end "/";

  -- Id: A.22
  function "/" (L, R: SIGNED) return SIGNED is
    constant L_LEFT: INTEGER := L'LENGTH-1;
    constant R_LEFT: INTEGER := R'LENGTH-1;
    alias XXL: SIGNED(L_LEFT downto 0) is L;
    alias XXR: SIGNED(R_LEFT downto 0) is R;
    variable XL: SIGNED(L_LEFT downto 0);
    variable XR: SIGNED(R_LEFT downto 0);
    variable FQUOT: UNSIGNED(L'LENGTH-1 downto 0);
    variable FREMAIN: UNSIGNED(R'LENGTH-1 downto 0);
    variable XNUM: UNSIGNED(L'LENGTH-1 downto 0);
    variable XDENOM: UNSIGNED(R'LENGTH-1 downto 0);
    variable QNEG: BOOLEAN := FALSE;
  begin
    if ((L'LENGTH < 1) or (R'LENGTH < 1)) then return NAS;
    end if;
    XL := TO_01(XXL, 'X');
    XR := TO_01(XXR, 'X');
    if ((XL(XL'LEFT)='X') or (XR(XR'LEFT)='X')) then
      FQUOT := (others => 'X');
      return SIGNED(FQUOT);
    end if;
    if XL(XL'LEFT)='1' then
      XNUM := UNSIGNED(-XL);
      QNEG := TRUE;
    else
      XNUM := UNSIGNED(XL);
    end if;
    if XR(XR'LEFT)='1' then
      XDENOM := UNSIGNED(-XR);
      QNEG := not QNEG;
    else
      XDENOM := UNSIGNED(XR);
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
    if L_LENGTH > R'LENGTH and QUOT(0)/='X'
        and QUOT(L_LENGTH-1 downto R'LENGTH)
        /= (L_LENGTH-1 downto R'LENGTH => '0')
        then
      assert NO_WARNING report "NUMERIC_STD.""/"": Quotient Truncated"
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
    if L_LENGTH > R'LENGTH and QUOT(0)/='X'
        and QUOT(L_LENGTH-1 downto R'LENGTH)
        /= (L_LENGTH-1 downto R'LENGTH => QUOT(R'LENGTH-1))
        then
      assert NO_WARNING report "NUMERIC_STD.""/"": Quotient Truncated"
          severity WARNING;
    end if;
    return RESIZE(QUOT, R'LENGTH);
  end "/";

  --============================================================================

  -- Id: A.27
  function "rem" (L, R: UNSIGNED) return UNSIGNED is
    constant L_LEFT: INTEGER := L'LENGTH-1;
    constant R_LEFT: INTEGER := R'LENGTH-1;
    alias XXL: UNSIGNED(L_LEFT downto 0) is L;
    alias XXR: UNSIGNED(R_LEFT downto 0) is R;
    variable XL: UNSIGNED(L_LEFT downto 0);
    variable XR: UNSIGNED(R_LEFT downto 0);
    variable FQUOT: UNSIGNED(L'LENGTH-1 downto 0);
    variable FREMAIN: UNSIGNED(R'LENGTH-1 downto 0);
  begin
    if ((L'LENGTH < 1) or (R'LENGTH < 1)) then return NAU;
    end if;
    XL := TO_01(XXL, 'X');
    XR := TO_01(XXR, 'X');
    if ((XL(XL'LEFT)='X') or (XR(XR'LEFT)='X')) then
      FREMAIN := (others => 'X');
      return FREMAIN;
    end if;
    DIVMOD(XL, XR, FQUOT, FREMAIN);
    return FREMAIN;
  end "rem";

  -- Id: A.28
  function "rem" (L, R: SIGNED) return SIGNED is
    constant L_LEFT: INTEGER := L'LENGTH-1;
    constant R_LEFT: INTEGER := R'LENGTH-1;
    alias XXL: SIGNED(L_LEFT downto 0) is L;
    alias XXR: SIGNED(R_LEFT downto 0) is R;
    variable FQUOT: UNSIGNED(L'LENGTH-1 downto 0);
    variable FREMAIN: UNSIGNED(R'LENGTH-1 downto 0);
    variable XNUM: UNSIGNED(L'LENGTH-1 downto 0);
    variable XDENOM: UNSIGNED(R'LENGTH-1 downto 0);
    variable RNEG: BOOLEAN := FALSE;
  begin
    if ((L'LENGTH < 1) or (R'LENGTH < 1)) then return NAS;
    end if;
    XNUM := UNSIGNED(TO_01(XXL, 'X'));
    XDENOM := UNSIGNED(TO_01(XXR, 'X'));
    if ((XNUM(XNUM'LEFT)='X') or (XDENOM(XDENOM'LEFT)='X')) then
      FREMAIN := (others => 'X');
      return SIGNED(FREMAIN);
    end if;
    if XNUM(XNUM'LEFT)='1' then
      XNUM := UNSIGNED(-SIGNED(XNUM));
      RNEG := TRUE;
    else
      XNUM := UNSIGNED(XNUM);
    end if;
    if XDENOM(XDENOM'LEFT)='1' then
      XDENOM := UNSIGNED(-SIGNED(XDENOM));
    else
      XDENOM := UNSIGNED(XDENOM);
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
    XREM := L rem XR;
    if R_LENGTH > L'LENGTH and XREM(0)/='X'
        and XREM(R_LENGTH-1 downto L'LENGTH)
        /= (R_LENGTH-1 downto L'LENGTH => '0')
        then
      assert NO_WARNING report "NUMERIC_STD.""rem"": Remainder Truncated"
          severity WARNING;
    end if;
    return RESIZE(XREM, L'LENGTH);
  end "rem";

  -- Id: A.30
  function "rem" (L: NATURAL; R: UNSIGNED) return UNSIGNED is
    constant L_LENGTH: NATURAL := MAX(UNSIGNED_NUM_BITS(L), R'LENGTH);
    variable XL, XREM: UNSIGNED(L_LENGTH-1 downto 0);
  begin
    XL := TO_UNSIGNED(L, L_LENGTH);
    XREM := XL rem R;
    if L_LENGTH > R'LENGTH and XREM(0)/='X'
        and XREM(L_LENGTH-1 downto R'LENGTH)
        /= (L_LENGTH-1 downto R'LENGTH => '0')
        then
      assert NO_WARNING report "NUMERIC_STD.""rem"": Remainder Truncated"
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
    if R_LENGTH > L'LENGTH and XREM(0)/='X'
        and XREM(R_LENGTH-1 downto L'LENGTH)
        /= (R_LENGTH-1 downto L'LENGTH => XREM(L'LENGTH-1))
        then
      assert NO_WARNING report "NUMERIC_STD.""rem"": Remainder Truncated"
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
    if L_LENGTH > R'LENGTH and XREM(0)/='X'
        and XREM(L_LENGTH-1 downto R'LENGTH)
        /= (L_LENGTH-1 downto R'LENGTH => XREM(R'LENGTH-1))
        then
      assert NO_WARNING report "NUMERIC_STD.""rem"": Remainder Truncated"
          severity WARNING;
    end if;
    return RESIZE(XREM, R'LENGTH);
  end "rem";

  --============================================================================

  -- Id: A.33
  function "mod" (L, R: UNSIGNED) return UNSIGNED is
    constant L_LEFT: INTEGER := L'LENGTH-1;
    constant R_LEFT: INTEGER := R'LENGTH-1;
    alias XXL: UNSIGNED(L_LEFT downto 0) is L;
    alias XXR: UNSIGNED(R_LEFT downto 0) is R;
    variable XL: UNSIGNED(L_LEFT downto 0);
    variable XR: UNSIGNED(R_LEFT downto 0);
    variable FQUOT: UNSIGNED(L'LENGTH-1 downto 0);
    variable FREMAIN: UNSIGNED(R'LENGTH-1 downto 0);
  begin
    if ((L'LENGTH < 1) or (R'LENGTH < 1)) then return NAU;
    end if;
    XL := TO_01(XXL, 'X');
    XR := TO_01(XXR, 'X');
    if ((XL(XL'LEFT)='X') or (XR(XR'LEFT)='X')) then
      FREMAIN := (others => 'X');
      return FREMAIN;
    end if;
    DIVMOD(XL, XR, FQUOT, FREMAIN);
    return FREMAIN;
  end "mod";

  -- Id: A.34
  function "mod" (L, R: SIGNED) return SIGNED is
    constant L_LEFT: INTEGER := L'LENGTH-1;
    constant R_LEFT: INTEGER := R'LENGTH-1;
    alias XXL: SIGNED(L_LEFT downto 0) is L;
    alias XXR: SIGNED(R_LEFT downto 0) is R;
    variable XL: SIGNED(L_LEFT downto 0);
    variable XR: SIGNED(R_LEFT downto 0);
    variable FQUOT: UNSIGNED(L'LENGTH-1 downto 0);
    variable FREMAIN: UNSIGNED(R'LENGTH-1 downto 0);
    variable XNUM: UNSIGNED(L'LENGTH-1 downto 0);
    variable XDENOM: UNSIGNED(R'LENGTH-1 downto 0);
    variable RNEG: BOOLEAN := FALSE;
  begin
    if ((L'LENGTH < 1) or (R'LENGTH < 1)) then return NAS;
    end if;
    XL := TO_01(XXL, 'X');
    XR := TO_01(XXR, 'X');
    if ((XL(XL'LEFT)='X') or (XR(XR'LEFT)='X')) then
      FREMAIN := (others => 'X');
      return SIGNED(FREMAIN);
    end if;
    if XL(XL'LEFT)='1' then
      XNUM := UNSIGNED(-XL);
    else
      XNUM := UNSIGNED(XL);
    end if;
    if XR(XR'LEFT)='1' then
      XDENOM := UNSIGNED(-XR);
      RNEG := TRUE;
    else
      XDENOM := UNSIGNED(XR);
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
    if R_LENGTH > L'LENGTH and XREM(0)/='X'
        and XREM(R_LENGTH-1 downto L'LENGTH)
        /= (R_LENGTH-1 downto L'LENGTH => '0')
        then
      assert NO_WARNING report "NUMERIC_STD.""mod"": Modulus Truncated"
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
    if L_LENGTH > R'LENGTH and XREM(0)/='X'
        and XREM(L_LENGTH-1 downto R'LENGTH)
        /= (L_LENGTH-1 downto R'LENGTH => '0')
        then
      assert NO_WARNING report "NUMERIC_STD.""mod"": Modulus Truncated"
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
    if R_LENGTH > L'LENGTH and XREM(0)/='X'
        and XREM(R_LENGTH-1 downto L'LENGTH)
        /= (R_LENGTH-1 downto L'LENGTH => XREM(L'LENGTH-1))
        then
      assert NO_WARNING report "NUMERIC_STD.""mod"": Modulus Truncated"
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
    if L_LENGTH > R'LENGTH and XREM(0)/='X'
        and XREM(L_LENGTH-1 downto R'LENGTH)
        /= (L_LENGTH-1 downto R'LENGTH => XREM(R'LENGTH-1))
        then
      assert NO_WARNING report "NUMERIC_STD.""mod"": Modulus Truncated"
          severity WARNING;
    end if;
    return RESIZE(XREM, R'LENGTH);
  end "mod";

  --============================================================================

  -- Id: C.1
  function ">" (L, R: UNSIGNED) return BOOLEAN is
    constant L_LEFT: INTEGER := L'LENGTH-1;
    constant R_LEFT: INTEGER := R'LENGTH-1;
    alias XL: UNSIGNED(L_LEFT downto 0) is L;
    alias XR: UNSIGNED(R_LEFT downto 0) is R;
    constant SIZE: NATURAL := MAX(L'LENGTH, R'LENGTH);
    variable L01 : UNSIGNED(L_LEFT downto 0);
    variable R01 : UNSIGNED(R_LEFT downto 0);
  begin
    if ((L'LENGTH < 1) or (R'LENGTH < 1)) then
      assert NO_WARNING
          report "NUMERIC_STD."">"": null argument detected, returning FALSE"
          severity WARNING;
      return FALSE;
    end if;
    L01 := TO_01(XL, 'X');
    R01 := TO_01(XR, 'X');
    if ((L01(L01'LEFT)='X') or (R01(R01'LEFT)='X')) then
      assert NO_WARNING
          report "NUMERIC_STD."">"": metavalue detected, returning FALSE"
          severity WARNING;
      return FALSE;
    end if;
    return not UNSIGNED_LESS_OR_EQUAL(RESIZE(L01, SIZE), RESIZE(R01, SIZE));
  end ">";

  -- Id: C.2
  function ">" (L, R: SIGNED) return BOOLEAN is
    constant L_LEFT: INTEGER := L'LENGTH-1;
    constant R_LEFT: INTEGER := R'LENGTH-1;
    alias XL: SIGNED(L_LEFT downto 0) is L;
    alias XR: SIGNED(R_LEFT downto 0) is R;
    constant SIZE: NATURAL := MAX(L'LENGTH, R'LENGTH);
    variable L01 : SIGNED(L_LEFT downto 0);
    variable R01 : SIGNED(R_LEFT downto 0);
  begin
    if ((L'LENGTH < 1) or (R'LENGTH < 1)) then
      assert NO_WARNING
          report "NUMERIC_STD."">"": null argument detected, returning FALSE"
          severity WARNING;
      return FALSE;
    end if;
    L01 := TO_01(XL, 'X');
    R01 := TO_01(XR, 'X');
    if ((L01(L01'LEFT)='X') or (R01(R01'LEFT)='X')) then
      assert NO_WARNING
          report "NUMERIC_STD."">"": metavalue detected, returning FALSE"
          severity WARNING;
      return FALSE;
    end if;
    return not SIGNED_LESS_OR_EQUAL(RESIZE(L01, SIZE), RESIZE(R01, SIZE));
  end ">";

  -- Id: C.3
  function ">" (L: NATURAL; R: UNSIGNED) return BOOLEAN is
    constant R_LEFT: INTEGER := R'LENGTH-1;
    alias XR: UNSIGNED(R_LEFT downto 0) is R;
    variable R01 : UNSIGNED(R_LEFT downto 0);
  begin
    if (R'LENGTH < 1) then
      assert NO_WARNING
          report "NUMERIC_STD."">"": null argument detected, returning FALSE"
          severity WARNING;
      return FALSE;
    end if;
    R01 := TO_01(XR, 'X');
    if (R01(R01'LEFT)='X') then
      assert NO_WARNING
          report "NUMERIC_STD."">"": metavalue detected, returning FALSE"
          severity WARNING;
      return FALSE;
    end if;
    if UNSIGNED_NUM_BITS(L) > R'LENGTH then return TRUE;
    end if;
    return not UNSIGNED_LESS_OR_EQUAL(TO_UNSIGNED(L, R01'LENGTH), R01);
  end ">";

  -- Id: C.4
  function ">" (L: INTEGER; R: SIGNED) return BOOLEAN is
    constant R_LEFT: INTEGER := R'LENGTH-1;
    alias XR: SIGNED(R_LEFT downto 0) is R;
    variable R01 : SIGNED(R_LEFT downto 0);
  begin
    if (R'LENGTH < 1) then
      assert NO_WARNING
          report "NUMERIC_STD."">"": null argument detected, returning FALSE"
          severity WARNING;
      return FALSE;
    end if;
    R01 := TO_01(XR, 'X');
    if (R01(R01'LEFT)='X') then
      assert NO_WARNING
          report "NUMERIC_STD."">"": metavalue detected, returning FALSE"
          severity WARNING;
      return FALSE;
    end if;
    if SIGNED_NUM_BITS(L) > R'LENGTH then return L > 0;
    end if;
    return not SIGNED_LESS_OR_EQUAL(TO_SIGNED(L, R01'LENGTH), R01);
  end ">";

  -- Id: C.5
  function ">" (L: UNSIGNED; R: NATURAL) return BOOLEAN is
    constant L_LEFT: INTEGER := L'LENGTH-1;
    alias XL: UNSIGNED(L_LEFT downto 0) is L;
    variable L01 : UNSIGNED(L_LEFT downto 0);
  begin
    if (L'LENGTH < 1) then
      assert NO_WARNING
          report "NUMERIC_STD."">"": null argument detected, returning FALSE"
          severity WARNING;
      return FALSE;
    end if;
    L01 := TO_01(XL, 'X');
    if (L01(L01'LEFT)='X') then
      assert NO_WARNING
          report "NUMERIC_STD."">"": metavalue detected, returning FALSE"
          severity WARNING;
      return FALSE;
    end if;
    if UNSIGNED_NUM_BITS(R) > L'LENGTH then return FALSE;
    end if;
    return not UNSIGNED_LESS_OR_EQUAL(L01, TO_UNSIGNED(R, L01'LENGTH));
  end ">";

  -- Id: C.6
  function ">" (L: SIGNED; R: INTEGER) return BOOLEAN is
    constant L_LEFT: INTEGER := L'LENGTH-1;
    alias XL: SIGNED(L_LEFT downto 0) is L;
    variable L01 : SIGNED(L_LEFT downto 0);
  begin
    if (L'LENGTH < 1) then
      assert NO_WARNING
          report "NUMERIC_STD."">"": null argument detected, returning FALSE"
          severity WARNING;
      return FALSE;
    end if;
    L01 := TO_01(XL, 'X');
    if (L01(L01'LEFT)='X') then
      assert NO_WARNING
          report "NUMERIC_STD."">"": metavalue detected, returning FALSE"
          severity WARNING;
      return FALSE;
    end if;
    if SIGNED_NUM_BITS(R) > L'LENGTH then return 0 > R;
    end if;
    return not SIGNED_LESS_OR_EQUAL(L01, TO_SIGNED(R, L01'LENGTH));
  end ">";

  --============================================================================

  -- Id: C.7
  function "<" (L, R: UNSIGNED) return BOOLEAN is
    constant L_LEFT: INTEGER := L'LENGTH-1;
    constant R_LEFT: INTEGER := R'LENGTH-1;
    alias XL: UNSIGNED(L_LEFT downto 0) is L;
    alias XR: UNSIGNED(R_LEFT downto 0) is R;
    constant SIZE: NATURAL := MAX(L'LENGTH, R'LENGTH);
    variable L01 : UNSIGNED(L_LEFT downto 0);
    variable R01 : UNSIGNED(R_LEFT downto 0);
  begin
    if ((L'LENGTH < 1) or (R'LENGTH < 1)) then
      assert NO_WARNING
          report "NUMERIC_STD.""<"": null argument detected, returning FALSE"
          severity WARNING;
      return FALSE;
    end if;
    L01 := TO_01(XL, 'X');
    R01 := TO_01(XR, 'X');
    if ((L01(L01'LEFT)='X') or (R01(R01'LEFT)='X')) then
      assert NO_WARNING
          report "NUMERIC_STD.""<"": metavalue detected, returning FALSE"
          severity WARNING;
      return FALSE;
    end if;
    return UNSIGNED_LESS(RESIZE(L01, SIZE), RESIZE(R01, SIZE));
  end "<";

  -- Id: C.8
  function "<" (L, R: SIGNED) return BOOLEAN is
    constant L_LEFT: INTEGER := L'LENGTH-1;
    constant R_LEFT: INTEGER := R'LENGTH-1;
    alias XL: SIGNED(L_LEFT downto 0) is L;
    alias XR: SIGNED(R_LEFT downto 0) is R;
    constant SIZE: NATURAL := MAX(L'LENGTH, R'LENGTH);
    variable L01 : SIGNED(L_LEFT downto 0);
    variable R01 : SIGNED(R_LEFT downto 0);
  begin
    if ((L'LENGTH < 1) or (R'LENGTH < 1)) then
      assert NO_WARNING
          report "NUMERIC_STD.""<"": null argument detected, returning FALSE"
          severity WARNING;
      return FALSE;
    end if;
    L01 := TO_01(XL, 'X');
    R01 := TO_01(XR, 'X');
    if ((L01(L01'LEFT)='X') or (R01(R01'LEFT)='X')) then
      assert NO_WARNING
          report "NUMERIC_STD.""<"": metavalue detected, returning FALSE"
          severity WARNING;
      return FALSE;
    end if;
    return SIGNED_LESS(RESIZE(L01, SIZE), RESIZE(R01, SIZE));
  end "<";

  -- Id: C.9
  function "<" (L: NATURAL; R: UNSIGNED) return BOOLEAN is
    constant R_LEFT: INTEGER := R'LENGTH-1;
    alias XR: UNSIGNED(R_LEFT downto 0) is R;
    variable R01 : UNSIGNED(R_LEFT downto 0);
  begin
    if (R'LENGTH < 1) then
      assert NO_WARNING
          report "NUMERIC_STD.""<"": null argument detected, returning FALSE"
          severity WARNING;
      return FALSE;
    end if;
    R01 := TO_01(XR, 'X');
    if (R01(R01'LEFT)='X') then
      assert NO_WARNING
          report "NUMERIC_STD.""<"": metavalue detected, returning FALSE"
          severity WARNING;
      return FALSE;
    end if;
    if UNSIGNED_NUM_BITS(L) > R'LENGTH then return L < 0;
    end if;
    return UNSIGNED_LESS(TO_UNSIGNED(L, R01'LENGTH), R01);
  end "<";

  -- Id: C.10
  function "<" (L: INTEGER; R: SIGNED) return BOOLEAN is
    constant R_LEFT: INTEGER := R'LENGTH-1;
    alias XR: SIGNED(R_LEFT downto 0) is R;
    variable R01 : SIGNED(R_LEFT downto 0);
  begin
    if (R'LENGTH < 1) then
      assert NO_WARNING
          report "NUMERIC_STD.""<"": null argument detected, returning FALSE"
          severity WARNING;
      return FALSE;
    end if;
    R01 := TO_01(XR, 'X');
    if (R01(R01'LEFT)='X') then
      assert NO_WARNING
          report "NUMERIC_STD.""<"": metavalue detected, returning FALSE"
          severity WARNING;
      return FALSE;
    end if;
    if SIGNED_NUM_BITS(L) > R'LENGTH then return L < 0;
    end if;
    return SIGNED_LESS(TO_SIGNED(L, R01'LENGTH), R01);
  end "<";

  -- Id: C.11
  function "<" (L: UNSIGNED; R: NATURAL) return BOOLEAN is
    constant L_LEFT: INTEGER := L'LENGTH-1;
    alias XL: UNSIGNED(L_LEFT downto 0) is L;
    variable L01 : UNSIGNED(L_LEFT downto 0);
  begin
    if (L'LENGTH < 1) then
      assert NO_WARNING
          report "NUMERIC_STD.""<"": null argument detected, returning FALSE"
          severity WARNING;
      return FALSE;
    end if;
    L01 := TO_01(XL, 'X');
    if (L01(L01'LEFT)='X') then
      assert NO_WARNING
          report "NUMERIC_STD.""<"": metavalue detected, returning FALSE"
          severity WARNING;
      return FALSE;
    end if;
    if UNSIGNED_NUM_BITS(R) > L'LENGTH then return 0 < R;
    end if;
    return UNSIGNED_LESS(L01, TO_UNSIGNED(R, L01'LENGTH));
  end "<";

  -- Id: C.12
  function "<" (L: SIGNED; R: INTEGER) return BOOLEAN is
    constant L_LEFT: INTEGER := L'LENGTH-1;
    alias XL: SIGNED(L_LEFT downto 0) is L;
    variable L01 : SIGNED(L_LEFT downto 0);
  begin
    if (L'LENGTH < 1) then
      assert NO_WARNING
          report "NUMERIC_STD.""<"": null argument detected, returning FALSE"
          severity WARNING;
      return FALSE;
    end if;
    L01 := TO_01(XL, 'X');
    if (L01(L01'LEFT)='X') then
      assert NO_WARNING
          report "NUMERIC_STD.""<"": metavalue detected, returning FALSE"
          severity WARNING;
      return FALSE;
    end if;
    if SIGNED_NUM_BITS(R) > L'LENGTH then return 0 < R;
    end if;
    return SIGNED_LESS(L01, TO_SIGNED(R, L01'LENGTH));
  end "<";

  --============================================================================

  -- Id: C.13
  function "<=" (L, R: UNSIGNED) return BOOLEAN is
    constant L_LEFT: INTEGER := L'LENGTH-1;
    constant R_LEFT: INTEGER := R'LENGTH-1;
    alias XL: UNSIGNED(L_LEFT downto 0) is L;
    alias XR: UNSIGNED(R_LEFT downto 0) is R;
    constant SIZE: NATURAL := MAX(L'LENGTH, R'LENGTH);
    variable L01 : UNSIGNED(L_LEFT downto 0);
    variable R01 : UNSIGNED(R_LEFT downto 0);
  begin
    if ((L'LENGTH < 1) or (R'LENGTH < 1)) then
      assert NO_WARNING
          report "NUMERIC_STD.""<="": null argument detected, returning FALSE"
          severity WARNING;
      return FALSE;
    end if;
    L01 := TO_01(XL, 'X');
    R01 := TO_01(XR, 'X');
    if ((L01(L01'LEFT)='X') or (R01(R01'LEFT)='X')) then
      assert NO_WARNING
          report "NUMERIC_STD.""<="": metavalue detected, returning FALSE"
          severity WARNING;
      return FALSE;
    end if;
    return UNSIGNED_LESS_OR_EQUAL(RESIZE(L01, SIZE), RESIZE(R01, SIZE));
  end "<=";

  -- Id: C.14
  function "<=" (L, R: SIGNED) return BOOLEAN is
    constant L_LEFT: INTEGER := L'LENGTH-1;
    constant R_LEFT: INTEGER := R'LENGTH-1;
    alias XL: SIGNED(L_LEFT downto 0) is L;
    alias XR: SIGNED(R_LEFT downto 0) is R;
    constant SIZE: NATURAL := MAX(L'LENGTH, R'LENGTH);
    variable L01 : SIGNED(L_LEFT downto 0);
    variable R01 : SIGNED(R_LEFT downto 0);
  begin
    if ((L'LENGTH < 1) or (R'LENGTH < 1)) then
      assert NO_WARNING
          report "NUMERIC_STD.""<="": null argument detected, returning FALSE"
          severity WARNING;
      return FALSE;
    end if;
    L01 := TO_01(XL, 'X');
    R01 := TO_01(XR, 'X');
    if ((L01(L01'LEFT)='X') or (R01(R01'LEFT)='X')) then
      assert NO_WARNING
          report "NUMERIC_STD.""<="": metavalue detected, returning FALSE"
          severity WARNING;
      return FALSE;
    end if;
    return SIGNED_LESS_OR_EQUAL(RESIZE(L01, SIZE), RESIZE(R01, SIZE));
  end "<=";

  -- Id: C.15
  function "<=" (L: NATURAL; R: UNSIGNED) return BOOLEAN is
    constant R_LEFT: INTEGER := R'LENGTH-1;
    alias XR: UNSIGNED(R_LEFT downto 0) is R;
    variable R01 : UNSIGNED(R_LEFT downto 0);
  begin
    if (R'LENGTH < 1) then
      assert NO_WARNING
          report "NUMERIC_STD.""<="": null argument detected, returning FALSE"
          severity WARNING;
      return FALSE;
    end if;
    R01 := TO_01(XR, 'X');
    if (R01(R01'LEFT)='X') then
      assert NO_WARNING
          report "NUMERIC_STD.""<="": metavalue detected, returning FALSE"
          severity WARNING;
      return FALSE;
    end if;
    if UNSIGNED_NUM_BITS(L) > R'LENGTH then return L < 0;
    end if;
    return UNSIGNED_LESS_OR_EQUAL(TO_UNSIGNED(L, R01'LENGTH), R01);
  end "<=";

  -- Id: C.16
  function "<=" (L: INTEGER; R: SIGNED) return BOOLEAN is
    constant R_LEFT: INTEGER := R'LENGTH-1;
    alias XR: SIGNED(R_LEFT downto 0) is R;
    variable R01 : SIGNED(R_LEFT downto 0);
  begin
    if (R'LENGTH < 1) then
      assert NO_WARNING
          report "NUMERIC_STD.""<="": null argument detected, returning FALSE"
          severity WARNING;
      return FALSE;
    end if;
    R01 := TO_01(XR, 'X');
    if (R01(R01'LEFT)='X') then
      assert NO_WARNING
          report "NUMERIC_STD.""<="": metavalue detected, returning FALSE"
          severity WARNING;
      return FALSE;
    end if;
    if SIGNED_NUM_BITS(L) > R'LENGTH then return L < 0;
    end if;
    return SIGNED_LESS_OR_EQUAL(TO_SIGNED(L, R01'LENGTH), R01);
  end "<=";

  -- Id: C.17
  function "<=" (L: UNSIGNED; R: NATURAL) return BOOLEAN is
    constant L_LEFT: INTEGER := L'LENGTH-1;
    alias XL: UNSIGNED(L_LEFT downto 0) is L;
    variable L01 : UNSIGNED(L_LEFT downto 0);
  begin
    if (L_LEFT < 0) then
      assert NO_WARNING
          report "NUMERIC_STD.""<="": null argument detected, returning FALSE"
          severity WARNING;
      return FALSE;
    end if;
    L01 := TO_01(XL, 'X');
    if (L01(L01'LEFT)='X') then
      assert NO_WARNING
          report "NUMERIC_STD.""<="": metavalue detected, returning FALSE"
          severity WARNING;
      return FALSE;
    end if;
    if UNSIGNED_NUM_BITS(R) > L'LENGTH then return 0 < R;
    end if;
    return UNSIGNED_LESS_OR_EQUAL(L01, TO_UNSIGNED(R, L01'LENGTH));
  end "<=";

  -- Id: C.18
  function "<=" (L: SIGNED; R: INTEGER) return BOOLEAN is
    constant L_LEFT: INTEGER := L'LENGTH-1;
    alias XL: SIGNED(L_LEFT downto 0) is L;
    variable L01 : SIGNED(L_LEFT downto 0);
  begin
    if (L_LEFT < 0) then
      assert NO_WARNING
          report "NUMERIC_STD.""<="": null argument detected, returning FALSE"
          severity WARNING;
      return FALSE;
    end if;
    L01 := TO_01(XL, 'X');
    if (L01(L01'LEFT)='X') then
      assert NO_WARNING
          report "NUMERIC_STD.""<="": metavalue detected, returning FALSE"
          severity WARNING;
      return FALSE;
    end if;
    if SIGNED_NUM_BITS(R) > L'LENGTH then return 0 < R;
    end if;
    return SIGNED_LESS_OR_EQUAL(L01, TO_SIGNED(R, L01'LENGTH));
  end "<=";

  --============================================================================

  -- Id: C.19
  function ">=" (L, R: UNSIGNED) return BOOLEAN is
    constant L_LEFT: INTEGER := L'LENGTH-1;
    constant R_LEFT: INTEGER := R'LENGTH-1;
    alias XL: UNSIGNED(L_LEFT downto 0) is L;
    alias XR: UNSIGNED(R_LEFT downto 0) is R;
    constant SIZE: NATURAL := MAX(L'LENGTH, R'LENGTH);
    variable L01 : UNSIGNED(L_LEFT downto 0);
    variable R01 : UNSIGNED(R_LEFT downto 0);
  begin
    if ((L'LENGTH < 1) or (R'LENGTH < 1)) then
      assert NO_WARNING
          report "NUMERIC_STD."">="": null argument detected, returning FALSE"
          severity WARNING;
      return FALSE;
    end if;
    L01 := TO_01(XL, 'X');
    R01 := TO_01(XR, 'X');
    if ((L01(L01'LEFT)='X') or (R01(R01'LEFT)='X')) then
      assert NO_WARNING
          report "NUMERIC_STD."">="": metavalue detected, returning FALSE"
          severity WARNING;
      return FALSE;
    end if;
    return not UNSIGNED_LESS(RESIZE(L01, SIZE), RESIZE(R01, SIZE));
  end ">=";

  -- Id: C.20
  function ">=" (L, R: SIGNED) return BOOLEAN is
    constant L_LEFT: INTEGER := L'LENGTH-1;
    constant R_LEFT: INTEGER := R'LENGTH-1;
    alias XL: SIGNED(L_LEFT downto 0) is L;
    alias XR: SIGNED(R_LEFT downto 0) is R;
    constant SIZE: NATURAL := MAX(L'LENGTH, R'LENGTH);
    variable L01 : SIGNED(L_LEFT downto 0);
    variable R01 : SIGNED(R_LEFT downto 0);
  begin
    if ((L'LENGTH < 1) or (R'LENGTH < 1)) then
      assert NO_WARNING
          report "NUMERIC_STD."">="": null argument detected, returning FALSE"
          severity WARNING;
      return FALSE;
    end if;
    L01 := TO_01(XL, 'X');
    R01 := TO_01(XR, 'X');
    if ((L01(L01'LEFT)='X') or (R01(R01'LEFT)='X')) then
      assert NO_WARNING
          report "NUMERIC_STD."">="": metavalue detected, returning FALSE"
          severity WARNING;
      return FALSE;
    end if;
    return not SIGNED_LESS(RESIZE(L01, SIZE), RESIZE(R01, SIZE));
  end ">=";

  -- Id: C.21
  function ">=" (L: NATURAL; R: UNSIGNED) return BOOLEAN is
    constant R_LEFT: INTEGER := R'LENGTH-1;
    alias XR: UNSIGNED(R_LEFT downto 0) is R;
    variable R01 : UNSIGNED(R_LEFT downto 0);
  begin
    if (R'LENGTH < 1) then
      assert NO_WARNING
          report "NUMERIC_STD."">="": null argument detected, returning FALSE"
          severity WARNING;
      return FALSE;
    end if;
    R01 := TO_01(XR, 'X');
    if (R01(R01'LEFT)='X') then
      assert NO_WARNING
          report "NUMERIC_STD."">="": metavalue detected, returning FALSE"
          severity WARNING;
      return FALSE;
    end if;
    if UNSIGNED_NUM_BITS(L) > R'LENGTH then return L > 0;
    end if;
    return not UNSIGNED_LESS(TO_UNSIGNED(L, R01'LENGTH), R01);
  end ">=";

  -- Id: C.22
  function ">=" (L: INTEGER; R: SIGNED) return BOOLEAN is
    constant R_LEFT: INTEGER := R'LENGTH-1;
    alias XR: SIGNED(R_LEFT downto 0) is R;
    variable R01 : SIGNED(R_LEFT downto 0);
  begin
    if (R'LENGTH < 1) then
      assert NO_WARNING
          report "NUMERIC_STD."">="": null argument detected, returning FALSE"
          severity WARNING;
      return FALSE;
    end if;
    R01 := TO_01(XR, 'X');
    if (R01(R01'LEFT)='X') then
      assert NO_WARNING
          report "NUMERIC_STD."">="": metavalue detected, returning FALSE"
          severity WARNING;
      return FALSE;
    end if;
    if SIGNED_NUM_BITS(L) > R'LENGTH then return L > 0;
    end if;
    return not SIGNED_LESS(TO_SIGNED(L, R01'LENGTH), R01);
  end ">=";

  -- Id: C.23
  function ">=" (L: UNSIGNED; R: NATURAL) return BOOLEAN is
    constant L_LEFT: INTEGER := L'LENGTH-1;
    alias XL: UNSIGNED(L_LEFT downto 0) is L;
    variable L01 : UNSIGNED(L_LEFT downto 0);
  begin
    if (L'LENGTH < 1) then
      assert NO_WARNING
          report "NUMERIC_STD."">="": null argument detected, returning FALSE"
          severity WARNING;
      return FALSE;
    end if;
    L01 := TO_01(XL, 'X');
    if (L01(L01'LEFT)='X') then
      assert NO_WARNING
          report "NUMERIC_STD."">="": metavalue detected, returning FALSE"
          severity WARNING;
      return FALSE;
    end if;
    if UNSIGNED_NUM_BITS(R) > L'LENGTH then return 0 > R;
    end if;
    return not UNSIGNED_LESS(L01, TO_UNSIGNED(R, L01'LENGTH));
  end ">=";

  -- Id: C.24
  function ">=" (L: SIGNED; R: INTEGER) return BOOLEAN is
    constant L_LEFT: INTEGER := L'LENGTH-1;
    alias XL: SIGNED(L_LEFT downto 0) is L;
    variable L01 : SIGNED(L_LEFT downto 0);
  begin
    if (L'LENGTH < 1) then
      assert NO_WARNING
          report "NUMERIC_STD."">="": null argument detected, returning FALSE"
          severity WARNING;
      return FALSE;
    end if;
    L01 := TO_01(XL, 'X');
    if (L01(L01'LEFT)='X') then
      assert NO_WARNING
          report "NUMERIC_STD."">="": metavalue detected, returning FALSE"
          severity WARNING;
      return FALSE;
    end if;
    if SIGNED_NUM_BITS(R) > L'LENGTH then return 0 > R;
    end if;
    return not SIGNED_LESS(L01, TO_SIGNED(R, L01'LENGTH));
  end ">=";

  --============================================================================

  -- Id: C.25
  function "=" (L, R: UNSIGNED) return BOOLEAN is
    constant L_LEFT: INTEGER := L'LENGTH-1;
    constant R_LEFT: INTEGER := R'LENGTH-1;
    alias XL: UNSIGNED(L_LEFT downto 0) is L;
    alias XR: UNSIGNED(R_LEFT downto 0) is R;
    constant SIZE: NATURAL := MAX(L'LENGTH, R'LENGTH);
    variable L01 : UNSIGNED(L_LEFT downto 0);
    variable R01 : UNSIGNED(R_LEFT downto 0);
  begin
    if ((L'LENGTH < 1) or (R'LENGTH < 1)) then
      assert NO_WARNING
          report "NUMERIC_STD.""="": null argument detected, returning FALSE"
          severity WARNING;
      return FALSE;
    end if;
    L01 := TO_01(XL, 'X');
    R01 := TO_01(XR, 'X');
    if ((L01(L01'LEFT)='X') or (R01(R01'LEFT)='X')) then
      assert NO_WARNING
          report "NUMERIC_STD.""="": metavalue detected, returning FALSE"
          severity WARNING;
      return FALSE;
    end if;
    return UNSIGNED_EQUAL(RESIZE(L01, SIZE), RESIZE(R01, SIZE));
  end "=";

  -- Id: C.26
  function "=" (L, R: SIGNED) return BOOLEAN is
    constant L_LEFT: INTEGER := L'LENGTH-1;
    constant R_LEFT: INTEGER := R'LENGTH-1;
    alias XL: SIGNED(L_LEFT downto 0) is L;
    alias XR: SIGNED(R_LEFT downto 0) is R;
    constant SIZE: NATURAL := MAX(L'LENGTH, R'LENGTH);
    variable L01 : SIGNED(L_LEFT downto 0);
    variable R01 : SIGNED(R_LEFT downto 0);
  begin
    if ((L'LENGTH < 1) or (R'LENGTH < 1)) then
      assert NO_WARNING
          report "NUMERIC_STD.""="": null argument detected, returning FALSE"
          severity WARNING;
      return FALSE;
    end if;
    L01 := TO_01(XL, 'X');
    R01 := TO_01(XR, 'X');
    if ((L01(L01'LEFT)='X') or (R01(R01'LEFT)='X')) then
      assert NO_WARNING
          report "NUMERIC_STD.""="": metavalue detected, returning FALSE"
          severity WARNING;
      return FALSE;
    end if;
    return SIGNED_EQUAL(RESIZE(L01, SIZE), RESIZE(R01, SIZE));
  end "=";

  -- Id: C.27
  function "=" (L: NATURAL; R: UNSIGNED) return BOOLEAN is
    constant R_LEFT: INTEGER := R'LENGTH-1;
    alias XR: UNSIGNED(R_LEFT downto 0) is R;
    variable R01 : UNSIGNED(R_LEFT downto 0);
  begin
    if (R'LENGTH < 1) then
      assert NO_WARNING
          report "NUMERIC_STD.""="": null argument detected, returning FALSE"
          severity WARNING;
      return FALSE;
    end if;
    R01 := TO_01(XR, 'X');
    if (R01(R01'LEFT)='X') then
      assert NO_WARNING
          report "NUMERIC_STD.""="": metavalue detected, returning FALSE"
          severity WARNING;
      return FALSE;
    end if;
    if UNSIGNED_NUM_BITS(L) > R'LENGTH then return FALSE;
    end if;
    return UNSIGNED_EQUAL(TO_UNSIGNED(L, R01'LENGTH), R01);
  end "=";

  -- Id: C.28
  function "=" (L: INTEGER; R: SIGNED) return BOOLEAN is
    constant R_LEFT: INTEGER := R'LENGTH-1;
    alias XR: SIGNED(R_LEFT downto 0) is R;
    variable R01 : SIGNED(R_LEFT downto 0);
  begin
    if (R'LENGTH < 1) then
      assert NO_WARNING
          report "NUMERIC_STD.""="": null argument detected, returning FALSE"
          severity WARNING;
      return FALSE;
    end if;
    R01 := TO_01(XR, 'X');
    if (R01(R01'LEFT)='X') then
      assert NO_WARNING
          report "NUMERIC_STD.""="": metavalue detected, returning FALSE"
          severity WARNING;
      return FALSE;
    end if;
    if SIGNED_NUM_BITS(L) > R'LENGTH then return FALSE;
    end if;
    return SIGNED_EQUAL(TO_SIGNED(L, R01'LENGTH), R01);
  end "=";

  -- Id: C.29
  function "=" (L: UNSIGNED; R: NATURAL) return BOOLEAN is
    constant L_LEFT: INTEGER := L'LENGTH-1;
    alias XL: UNSIGNED(L_LEFT downto 0) is L;
    variable L01 : UNSIGNED(L_LEFT downto 0);
  begin
    if (L'LENGTH < 1) then
      assert NO_WARNING
          report "NUMERIC_STD.""="": null argument detected, returning FALSE"
          severity WARNING;
      return FALSE;
    end if;
    L01 := TO_01(XL, 'X');
    if (L01(L01'LEFT)='X') then
      assert NO_WARNING
          report "NUMERIC_STD.""="": metavalue detected, returning FALSE"
          severity WARNING;
      return FALSE;
    end if;
    if UNSIGNED_NUM_BITS(R) > L'LENGTH then return FALSE;
    end if;
    return UNSIGNED_EQUAL(L01, TO_UNSIGNED(R, L01'LENGTH));
  end "=";

  -- Id: C.30
  function "=" (L: SIGNED; R: INTEGER) return BOOLEAN is
    constant L_LEFT: INTEGER := L'LENGTH-1;
    alias XL: SIGNED(L_LEFT downto 0) is L;
    variable L01 : SIGNED(L_LEFT downto 0);
  begin
    if (L'LENGTH < 1) then
      assert NO_WARNING
          report "NUMERIC_STD.""="": null argument detected, returning FALSE"
          severity WARNING;
      return FALSE;
    end if;
    L01 := TO_01(XL, 'X');
    if (L01(L01'LEFT)='X') then
      assert NO_WARNING
          report "NUMERIC_STD.""="": metavalue detected, returning FALSE"
          severity WARNING;
      return FALSE;
    end if;
    if SIGNED_NUM_BITS(R) > L'LENGTH then return FALSE;
    end if;
    return SIGNED_EQUAL(L01, TO_SIGNED(R, L01'LENGTH));
  end "=";

  --============================================================================

  -- Id: C.31
  function "/=" (L, R: UNSIGNED) return BOOLEAN is
    constant L_LEFT: INTEGER := L'LENGTH-1;
    constant R_LEFT: INTEGER := R'LENGTH-1;
    alias XL: UNSIGNED(L_LEFT downto 0) is L;
    alias XR: UNSIGNED(R_LEFT downto 0) is R;
    constant SIZE: NATURAL := MAX(L'LENGTH, R'LENGTH);
    variable L01 : UNSIGNED(L_LEFT downto 0);
    variable R01 : UNSIGNED(R_LEFT downto 0);
  begin
    if ((L'LENGTH < 1) or (R'LENGTH < 1)) then
      assert NO_WARNING
          report "NUMERIC_STD.""/="": null argument detected, returning TRUE"
          severity WARNING;
      return TRUE;
    end if;
    L01 := TO_01(XL, 'X');
    R01 := TO_01(XR, 'X');
    if ((L01(L01'LEFT)='X') or (R01(R01'LEFT)='X')) then
      assert NO_WARNING
          report "NUMERIC_STD.""/="": metavalue detected, returning TRUE"
          severity WARNING;
      return TRUE;
    end if;
    return not(UNSIGNED_EQUAL(RESIZE(L01, SIZE), RESIZE(R01, SIZE)));
  end "/=";

  -- Id: C.32
  function "/=" (L, R: SIGNED) return BOOLEAN is
    constant L_LEFT: INTEGER := L'LENGTH-1;
    constant R_LEFT: INTEGER := R'LENGTH-1;
    alias XL: SIGNED(L_LEFT downto 0) is L;
    alias XR: SIGNED(R_LEFT downto 0) is R;
    constant SIZE: NATURAL := MAX(L'LENGTH, R'LENGTH);
    variable L01 : SIGNED(L_LEFT downto 0);
    variable R01 : SIGNED(R_LEFT downto 0);
  begin
    if ((L'LENGTH < 1) or (R'LENGTH < 1)) then
      assert NO_WARNING
          report "NUMERIC_STD.""/="": null argument detected, returning TRUE"
          severity WARNING;
      return TRUE;
    end if;
    L01 := TO_01(XL, 'X');
    R01 := TO_01(XR, 'X');
    if ((L01(L01'LEFT)='X') or (R01(R01'LEFT)='X')) then
      assert NO_WARNING
          report "NUMERIC_STD.""/="": metavalue detected, returning TRUE"
          severity WARNING;
      return TRUE;
    end if;
    return not(SIGNED_EQUAL(RESIZE(L01, SIZE), RESIZE(R01, SIZE)));
  end "/=";

  -- Id: C.33
  function "/=" (L: NATURAL; R: UNSIGNED) return BOOLEAN is
    constant R_LEFT: INTEGER := R'LENGTH-1;
    alias XR: UNSIGNED(R_LEFT downto 0) is R;
    variable R01 : UNSIGNED(R_LEFT downto 0);
  begin
    if (R'LENGTH < 1) then
      assert NO_WARNING
          report "NUMERIC_STD.""/="": null argument detected, returning TRUE"
          severity WARNING;
      return TRUE;
    end if;
    R01 := TO_01(XR, 'X');
    if (R01(R01'LEFT)='X') then
      assert NO_WARNING
          report "NUMERIC_STD.""/="": metavalue detected, returning TRUE"
          severity WARNING;
      return TRUE;
    end if;
    if UNSIGNED_NUM_BITS(L) > R'LENGTH then return TRUE;
    end if;
    return not(UNSIGNED_EQUAL(TO_UNSIGNED(L, R01'LENGTH), R01));
  end "/=";

  -- Id: C.34
  function "/=" (L: INTEGER; R: SIGNED) return BOOLEAN is
    constant R_LEFT: INTEGER := R'LENGTH-1;
    alias XR: SIGNED(R_LEFT downto 0) is R;
    variable R01 : SIGNED(R_LEFT downto 0);
  begin
    if (R'LENGTH < 1) then
      assert NO_WARNING
          report "NUMERIC_STD.""/="": null argument detected, returning TRUE"
          severity WARNING;
      return TRUE;
    end if;
    R01 := TO_01(XR, 'X');
    if (R01(R01'LEFT)='X') then
      assert NO_WARNING
          report "NUMERIC_STD.""/="": metavalue detected, returning TRUE"
          severity WARNING;
      return TRUE;
    end if;
    if SIGNED_NUM_BITS(L) > R'LENGTH then return TRUE;
    end if;
    return not(SIGNED_EQUAL(TO_SIGNED(L, R01'LENGTH), R01));
  end "/=";

  -- Id: C.35
  function "/=" (L: UNSIGNED; R: NATURAL) return BOOLEAN is
    constant L_LEFT: INTEGER := L'LENGTH-1;
    alias XL: UNSIGNED(L_LEFT downto 0) is L;
    variable L01 : UNSIGNED(L_LEFT downto 0);
  begin
    if (L'LENGTH < 1) then
      assert NO_WARNING
          report "NUMERIC_STD.""/="": null argument detected, returning TRUE"
          severity WARNING;
      return TRUE;
    end if;
    L01 := TO_01(XL, 'X');
    if (L01(L01'LEFT)='X') then
      assert NO_WARNING
          report "NUMERIC_STD.""/="": metavalue detected, returning TRUE"
          severity WARNING;
      return TRUE;
    end if;
    if UNSIGNED_NUM_BITS(R) > L'LENGTH then return TRUE;
    end if;
    return not(UNSIGNED_EQUAL(L01, TO_UNSIGNED(R, L01'LENGTH)));
  end "/=";

  -- Id: C.36
  function "/=" (L: SIGNED; R: INTEGER) return BOOLEAN is
    constant L_LEFT: INTEGER := L'LENGTH-1;
    alias XL: SIGNED(L_LEFT downto 0) is L;
    variable L01 : SIGNED(L_LEFT downto 0);
  begin
    if (L'LENGTH < 1) then
      assert NO_WARNING
          report "NUMERIC_STD.""/="": null argument detected, returning TRUE"
          severity WARNING;
      return TRUE;
    end if;
    L01 := TO_01(XL, 'X');
    if (L01(L01'LEFT)='X') then
      assert NO_WARNING
          report "NUMERIC_STD.""/="": metavalue detected, returning TRUE"
          severity WARNING;
      return TRUE;
    end if;
    if SIGNED_NUM_BITS(R) > L'LENGTH then return TRUE;
    end if;
    return not(SIGNED_EQUAL(L01, TO_SIGNED(R, L01'LENGTH)));
  end "/=";

  --============================================================================

  -- Id: S.1
  function SHIFT_LEFT (ARG: UNSIGNED; COUNT: NATURAL) return UNSIGNED is
  begin
    if (ARG'LENGTH < 1) then return NAU;
    end if;
    return UNSIGNED(XSLL(STD_LOGIC_VECTOR(ARG), COUNT));
  end SHIFT_LEFT;

  -- Id: S.2
  function SHIFT_RIGHT (ARG: UNSIGNED; COUNT: NATURAL) return UNSIGNED is
  begin
    if (ARG'LENGTH < 1) then return NAU;
    end if;
    return UNSIGNED(XSRL(STD_LOGIC_VECTOR(ARG), COUNT));
  end SHIFT_RIGHT;

  -- Id: S.3
  function SHIFT_LEFT (ARG: SIGNED; COUNT: NATURAL) return SIGNED is
  begin
    if (ARG'LENGTH < 1) then return NAS;
    end if;
    return SIGNED(XSLL(STD_LOGIC_VECTOR(ARG), COUNT));
  end SHIFT_LEFT;

  -- Id: S.4
  function SHIFT_RIGHT (ARG: SIGNED; COUNT: NATURAL) return SIGNED is
  begin
    if (ARG'LENGTH < 1) then return NAS;
    end if;
    return SIGNED(XSRA(STD_LOGIC_VECTOR(ARG), COUNT));
  end SHIFT_RIGHT;

  --============================================================================

  -- Id: S.5
  function ROTATE_LEFT (ARG: UNSIGNED; COUNT: NATURAL) return UNSIGNED is
  begin
    if (ARG'LENGTH < 1) then return NAU;
    end if;
    return UNSIGNED(XROL(STD_LOGIC_VECTOR(ARG), COUNT));
  end ROTATE_LEFT;

  -- Id: S.6
  function ROTATE_RIGHT (ARG: UNSIGNED; COUNT: NATURAL) return UNSIGNED is
  begin
    if (ARG'LENGTH < 1) then return NAU;
    end if;
    return UNSIGNED(XROR(STD_LOGIC_VECTOR(ARG), COUNT));
  end ROTATE_RIGHT;


  -- Id: S.7
  function ROTATE_LEFT (ARG: SIGNED; COUNT: NATURAL) return SIGNED is
  begin
    if (ARG'LENGTH < 1) then return NAS;
    end if;
    return SIGNED(XROL(STD_LOGIC_VECTOR(ARG), COUNT));
  end ROTATE_LEFT;

  -- Id: S.8
  function ROTATE_RIGHT (ARG: SIGNED; COUNT: NATURAL) return SIGNED is
  begin
    if (ARG'LENGTH < 1) then return NAS;
    end if;
    return SIGNED(XROR(STD_LOGIC_VECTOR(ARG), COUNT));
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
  --   Note : Function S.10 is not compatible with VHDL 1076-1987. Comment
  --   out the function (declaration and body) for VHDL 1076-1987 compatibility.
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
  --   Note : Function S.11 is not compatible with VHDL 1076-1987. Comment
  --   out the function (declaration and body) for VHDL 1076-1987 compatibility.
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
  --   Note : Function S.12 is not compatible with VHDL 1076-1987. Comment
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
  --   out the function (declaration and body) for VHDL 1076-1987 compatibility.
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
  --   Note : Function S.14 is not compatible with VHDL 1076-1987. Comment
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
  --   Note : Function S.15 is not compatible with VHDL 1076-1987. Comment
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
    alias XXARG: UNSIGNED(ARG_LEFT downto 0) is ARG;
    variable XARG: UNSIGNED(ARG_LEFT downto 0);
    variable RESULT: NATURAL := 0;
  begin
    if (ARG'LENGTH < 1) then
      assert NO_WARNING
          report "NUMERIC_STD.TO_INTEGER: null detected, returning 0"
          severity WARNING;
      return 0;
    end if;
    XARG := TO_01(XXARG, 'X');
    if (XARG(XARG'LEFT)='X') then
      assert NO_WARNING
          report "NUMERIC_STD.TO_INTEGER: metavalue detected, returning 0"
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
    variable XARG: SIGNED(ARG'LENGTH-1 downto 0);
  begin
    if (ARG'LENGTH < 1) then
      assert NO_WARNING
          report "NUMERIC_STD.TO_INTEGER: null detected, returning 0"
          severity WARNING;
      return 0;
    end if;
    XARG := TO_01(ARG, 'X');
    if (XARG(XARG'LEFT)='X') then
      assert NO_WARNING
          report "NUMERIC_STD.TO_INTEGER: metavalue detected, returning 0"
          severity WARNING;
      return 0;
    end if;
    if XARG(XARG'LEFT) = '0' then
      return TO_INTEGER(UNSIGNED(XARG));
    else
      return (- (TO_INTEGER(UNSIGNED(- (XARG + 1)))) -1);
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
          report "NUMERIC_STD.TO_UNSIGNED: vector truncated"
          severity WARNING;
    end if;
    return RESULT;
  end TO_UNSIGNED;

  -- Id: D.4
  function TO_SIGNED (ARG: INTEGER; SIZE: NATURAL) return SIGNED is
    variable RESULT: SIGNED(SIZE-1 downto 0);
    variable B_VAL: STD_LOGIC := '0';
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
          report "NUMERIC_STD.TO_SIGNED: vector truncated"
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
    RESULT := UNSIGNED(not(STD_LOGIC_VECTOR(L)));
    return RESULT;
  end "not";

  -- Id: L.2
  function "and" (L, R: UNSIGNED) return UNSIGNED is
    variable RESULT: UNSIGNED(L'LENGTH-1 downto 0);
  begin
    RESULT := UNSIGNED(STD_LOGIC_VECTOR(L) and STD_LOGIC_VECTOR(R));
    return RESULT;
  end "and";

  -- Id: L.3
  function "or" (L, R: UNSIGNED) return UNSIGNED is
    variable RESULT: UNSIGNED(L'LENGTH-1 downto 0);
  begin
    RESULT := UNSIGNED(STD_LOGIC_VECTOR(L) or STD_LOGIC_VECTOR(R));
    return RESULT;
  end "or";

  -- Id: L.4
  function "nand" (L, R: UNSIGNED) return UNSIGNED is
    variable RESULT: UNSIGNED(L'LENGTH-1 downto 0);
  begin
    RESULT := UNSIGNED(STD_LOGIC_VECTOR(L) nand STD_LOGIC_VECTOR(R));
    return RESULT;
  end "nand";

  -- Id: L.5
  function "nor" (L, R: UNSIGNED) return UNSIGNED is
    variable RESULT: UNSIGNED(L'LENGTH-1 downto 0);
  begin
    RESULT := UNSIGNED(STD_LOGIC_VECTOR(L) nor STD_LOGIC_VECTOR(R));
    return RESULT;
  end "nor";

  -- Id: L.6
  function "xor" (L, R: UNSIGNED) return UNSIGNED is
    variable RESULT: UNSIGNED(L'LENGTH-1 downto 0);
  begin
    RESULT := UNSIGNED(STD_LOGIC_VECTOR(L) xor STD_LOGIC_VECTOR(R));
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
    RESULT := UNSIGNED(STD_LOGIC_VECTOR(L) xnor STD_LOGIC_VECTOR(R));
    return RESULT;
  end "xnor";
--END-V93

  -- Id: L.8
  function "not" (L: SIGNED) return SIGNED is
    variable RESULT: SIGNED(L'LENGTH-1 downto 0);
  begin
    RESULT := SIGNED(not(STD_LOGIC_VECTOR(L)));
    return RESULT;
  end "not";

  -- Id: L.9
  function "and" (L, R: SIGNED) return SIGNED is
    variable RESULT: SIGNED(L'LENGTH-1 downto 0);
  begin
    RESULT := SIGNED(STD_LOGIC_VECTOR(L) and STD_LOGIC_VECTOR(R));
    return RESULT;
  end "and";

  -- Id: L.10
  function "or" (L, R: SIGNED) return SIGNED is
    variable RESULT: SIGNED(L'LENGTH-1 downto 0);
  begin
    RESULT := SIGNED(STD_LOGIC_VECTOR(L) or STD_LOGIC_VECTOR(R));
    return RESULT;
  end "or";

  -- Id: L.11
  function "nand" (L, R: SIGNED) return SIGNED is
    variable RESULT: SIGNED(L'LENGTH-1 downto 0);
  begin
    RESULT := SIGNED(STD_LOGIC_VECTOR(L) nand STD_LOGIC_VECTOR(R));
    return RESULT;
  end "nand";

  -- Id: L.12
  function "nor" (L, R: SIGNED) return SIGNED is
    variable RESULT: SIGNED(L'LENGTH-1 downto 0);
  begin
    RESULT := SIGNED(STD_LOGIC_VECTOR(L) nor STD_LOGIC_VECTOR(R));
    return RESULT;
  end "nor";

  -- Id: L.13
  function "xor" (L, R: SIGNED) return SIGNED is
    variable RESULT: SIGNED(L'LENGTH-1 downto 0);
  begin
    RESULT := SIGNED(STD_LOGIC_VECTOR(L) xor STD_LOGIC_VECTOR(R));
    return RESULT;
  end "xor";

--START-V93
  ------------------------------------------------------------------------------
  -- Note : Function L.14 is not compatible with VHDL 1076-1987. Comment
  --   out the function (declaration and body) for VHDL 1076-1987 compatibility.
  ------------------------------------------------------------------------------
  -- Id: L.14
  function "xnor" (L, R: SIGNED) return SIGNED is
    variable RESULT: SIGNED(L'LENGTH-1 downto 0);
  begin
    RESULT := SIGNED(STD_LOGIC_VECTOR(L) xnor STD_LOGIC_VECTOR(R));
    return RESULT;
  end "xnor";
--END-V93

  --============================================================================

  -- support constants for STD_MATCH:

  type BOOLEAN_TABLE is array(STD_ULOGIC, STD_ULOGIC) of BOOLEAN;

  constant MATCH_TABLE: BOOLEAN_TABLE := (
      --------------------------------------------------------------------------
      -- U      X      0      1      Z      W      L      H      -
      --------------------------------------------------------------------------
      (FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE,  TRUE), -- | U |
      (FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE,  TRUE), -- | X |
      (FALSE, FALSE,  TRUE, FALSE, FALSE, FALSE,  TRUE, FALSE,  TRUE), -- | 0 |
      (FALSE, FALSE, FALSE,  TRUE, FALSE, FALSE, FALSE,  TRUE,  TRUE), -- | 1 |
      (FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE,  TRUE), -- | Z |
      (FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE,  TRUE), -- | W |
      (FALSE, FALSE,  TRUE, FALSE, FALSE, FALSE,  TRUE, FALSE,  TRUE), -- | L |
      (FALSE, FALSE, FALSE,  TRUE, FALSE, FALSE, FALSE,  TRUE,  TRUE), -- | H |
      ( TRUE,  TRUE,  TRUE,  TRUE,  TRUE,  TRUE,  TRUE,  TRUE,  TRUE)  -- | - |
      );

  -- Id: M.1
  function STD_MATCH (L, R: STD_ULOGIC) return BOOLEAN is
    variable VALUE: STD_ULOGIC;
  begin
    return MATCH_TABLE(L, R);
  end STD_MATCH;

  -- Id: M.2
  function STD_MATCH (L, R: UNSIGNED) return BOOLEAN is
    alias LV: UNSIGNED(1 to L'LENGTH) is L;
    alias RV: UNSIGNED(1 to R'LENGTH) is R;
  begin
    if ((L'LENGTH < 1) or (R'LENGTH < 1)) then
      assert NO_WARNING
          report "NUMERIC_STD.STD_MATCH: null detected, returning FALSE"
          severity WARNING;
      return FALSE;
    end if;
    if LV'LENGTH /= RV'LENGTH then
      assert NO_WARNING
          report "NUMERIC_STD.STD_MATCH: L'LENGTH /= R'LENGTH, returning FALSE"
          severity WARNING;
      return FALSE;
    else
      for I in LV'LOW to LV'HIGH loop
        if not (MATCH_TABLE(LV(I), RV(I))) then
          return FALSE;
        end if;
      end loop;
      return TRUE;
    end if;
  end STD_MATCH;

  -- Id: M.3
  function STD_MATCH (L, R: SIGNED) return BOOLEAN is
    alias LV: SIGNED(1 to L'LENGTH) is L;
    alias RV: SIGNED(1 to R'LENGTH) is R;
  begin
    if ((L'LENGTH < 1) or (R'LENGTH < 1)) then
      assert NO_WARNING
          report "NUMERIC_STD.STD_MATCH: null detected, returning FALSE"
          severity WARNING;
      return FALSE;
    end if;
    if LV'LENGTH /= RV'LENGTH then
      assert NO_WARNING
          report "NUMERIC_STD.STD_MATCH: L'LENGTH /= R'LENGTH, returning FALSE"
          severity WARNING;
      return FALSE;
    else
      for I in LV'LOW to LV'HIGH loop
        if not (MATCH_TABLE(LV(I), RV(I))) then
          return FALSE;
        end if;
      end loop;
      return TRUE;
    end if;
  end STD_MATCH;

  -- Id: M.4
  function STD_MATCH (L, R: STD_LOGIC_VECTOR) return BOOLEAN is
    alias LV: STD_LOGIC_VECTOR(1 to L'LENGTH) is L;
    alias RV: STD_LOGIC_VECTOR(1 to R'LENGTH) is R;
  begin
    if ((L'LENGTH < 1) or (R'LENGTH < 1)) then
      assert NO_WARNING
          report "NUMERIC_STD.STD_MATCH: null detected, returning FALSE"
          severity WARNING;
      return FALSE;
    end if;
    if LV'LENGTH /= RV'LENGTH then
      assert NO_WARNING
          report "NUMERIC_STD.STD_MATCH: L'LENGTH /= R'LENGTH, returning FALSE"
          severity WARNING;
      return FALSE;
    else
      for I in LV'LOW to LV'HIGH loop
        if not (MATCH_TABLE(LV(I), RV(I))) then
          return FALSE;
        end if;
      end loop;
      return TRUE;
    end if;
  end STD_MATCH;

  -- Id: M.5
  function STD_MATCH (L, R: STD_ULOGIC_VECTOR) return BOOLEAN is
    alias LV: STD_ULOGIC_VECTOR(1 to L'LENGTH) is L;
    alias RV: STD_ULOGIC_VECTOR(1 to R'LENGTH) is R;
  begin
    if ((L'LENGTH < 1) or (R'LENGTH < 1)) then
      assert NO_WARNING
          report "NUMERIC_STD.STD_MATCH: null detected, returning FALSE"
          severity WARNING;
      return FALSE;
    end if;
    if LV'LENGTH /= RV'LENGTH then
      assert NO_WARNING
          report "NUMERIC_STD.STD_MATCH: L'LENGTH /= R'LENGTH, returning FALSE"
          severity WARNING;
      return FALSE;
    else
      for I in LV'LOW to LV'HIGH loop
        if not (MATCH_TABLE(LV(I), RV(I))) then
          return FALSE;
        end if;
      end loop;
      return TRUE;
    end if;
  end STD_MATCH;

  --============================================================================

  -- function TO_01 is used to convert vectors to the
  --          correct form for exported functions,
  --          and to report if there is an element which
  --          is not in (0, 1, H, L).

  -- Id: T.1
  function TO_01 (S: UNSIGNED; XMAP: STD_LOGIC := '0') return UNSIGNED is
    variable RESULT: UNSIGNED(S'LENGTH-1 downto 0);
    variable BAD_ELEMENT: BOOLEAN := FALSE;
    alias XS: UNSIGNED(S'LENGTH-1 downto 0) is S;
  begin
    if (S'LENGTH < 1) then
      assert NO_WARNING
          report "NUMERIC_STD.TO_01: null detected, returning NAU"
          severity WARNING;
      return NAU;
    end if;
    for I in RESULT'RANGE loop
      case XS(I) is
        when '0' | 'L' => RESULT(I) := '0';
        when '1' | 'H' => RESULT(I) := '1';
        when others => BAD_ELEMENT := TRUE;
      end case;
    end loop;
    if BAD_ELEMENT then
      for I in RESULT'RANGE loop
        RESULT(I) := XMAP; -- standard fixup
      end loop;
    end if;
    return RESULT;
  end TO_01;

  -- Id: T.2
  function TO_01 (S: SIGNED; XMAP: STD_LOGIC := '0') return SIGNED is
    variable RESULT: SIGNED(S'LENGTH-1 downto 0);
    variable BAD_ELEMENT: BOOLEAN := FALSE;
    alias XS: SIGNED(S'LENGTH-1 downto 0) is S;
  begin
    if (S'LENGTH < 1) then
      assert NO_WARNING
          report "NUMERIC_STD.TO_01: null detected, returning NAS"
          severity WARNING;
      return NAS;
    end if;
    for I in RESULT'RANGE loop
      case XS(I) is
        when '0' | 'L' => RESULT(I) := '0';
        when '1' | 'H' => RESULT(I) := '1';
        when others => BAD_ELEMENT := TRUE;
      end case;
    end loop;
    if BAD_ELEMENT then
      for I in RESULT'RANGE loop
        RESULT(I) := XMAP; -- standard fixup
      end loop;
    end if;
    return RESULT;
  end TO_01;

  --============================================================================

end NUMERIC_STD;
