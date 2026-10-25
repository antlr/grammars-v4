-- BDB0A01.A
--
--                             Grant of Unlimited Rights
--
--     Under contracts F33600-87-D-0337, F33600-84-D-0280, MDA903-79-C-0687,
--     F08630-91-C-0015, and DCA100-97-D-0025, the U.S. Government obtained
--     unlimited rights in the software and documentation contained herein.
--     Unlimited rights are defined in DFAR 252.227-7013(a)(19).  By making
--     this public release, the Government intends to confer upon all
--     recipients unlimited rights  equal to those held by the Government.
--     These rights include rights to use, duplicate, release or disclose the
--     released technical data and computer software in whole or in part, in
--     any manner and for any purpose whatsoever, and to have or permit others
--     to do so.
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
--      Check that Storage_Size may not be specified for a derived
--      access_to_object type.
--
--      Check that Storage_Pool may not be specified for a derived
--      access_to_object type.
--
--      Check that type Root_Storage_Pool is abstract, and requires
--      overriding definitions for procedures Allocate, Deallocate
--      and function Storage_Size.
--
--      Check that Storage_Size may not be specified for a given access
--      type if Storage_Pool is specified for it.
--
--
-- TEST DESCRIPTION:
--      This test attempts to specify both Storage_Pool and Storage_Size for
--      a single pool.  This test attempts to derive several different types
--      from System.Storage_Pools.Root_Storage_Pool, each time omitting one
--      of the required subprograms.
--
--      Assumes that the compilers will typically flag the missing subprogram
--      errors at the end, hence the OPTIONAL ERRORS at the type definitions,
--      where some compilers may choose to flag the error.
--
-- TEST FILES:
--      The following files comprise this test:
--
--         FDB0A00.A   (foundation code)
--         BDB0A01.A
--
--
-- PASS/FAIL CRITERIA:
--     This test contains several lines marked POSSIBLE ERROR: [Setnn].
--     For each value of n, the implementation must detect one or more of
--     these possible errors. For instance, an error must be detected on
--     at least one of the lines labeled POSSIBLE ERROR: [Set1] for an
--     implementation to pass.
--
--
-- CHANGE HISTORY:
--      09 AUG 95   SAIC   Initial version.
--      05 APR 96   SAIC   Fixed header for 2.1.
--      07 Feb 18   RLB    Added error location indicators and "Possible
--                         Error" to reflect common error reporting
--                         strategies (this required adding nested packages
--                         for some test cases to keep the errors separate).
--
--!

----------------------------------------------------------------- BDB0A01

with FDB0A00;
with System.Storage_Pools;
with System.Storage_Elements;
package BDB0A01 is

  type Access_Integer is access Integer;

  My_Pool   : FDB0A00.Stack_Heap(200);
  Your_Pool : FDB0A00.Stack_Heap(200);

  --------------- check attribute specification misuses

  for Access_Integer'Storage_Pool use My_Pool;                 -- OK.

  for Access_Integer'Storage_Size use 100;                     -- ERROR: {7;1}
     -- Storage_Size may not be specified for pool with Storage_Pool specified

  --      Check that Storage_Size may not be specified for a derived
  --      access_to_object type.

  type Second_Access_Integer is access Integer;

  type Derived_Access_Integer is new Second_Access_Integer;

  for Derived_Access_Integer'Storage_Pool use Your_Pool;       -- ERROR: {7;1}
           -- Storage_Size may only be specified for a non-derived access type

 --------------- check Storage_Pool derivation errors


  package Nest1 is
    type Missing_Allocate is
       new System.Storage_Pools.Root_Storage_Pool with null record;
                                              -- POSSIBLE ERROR: [Set1] {2:5}
         -- type Missing_Allocate: no override for abstract procedure Allocate

    procedure Deallocate(
      Pool : in out Missing_Allocate;
      Storage_Address : in System.Address;
      Size_In_Storage_Elements : in System.Storage_Elements.Storage_Count;
      Alignment : in System.Storage_Elements.Storage_Count);

    function Storage_Size( Pool: in Missing_Allocate )
             return System.Storage_Elements.Storage_Count;

    -- type Missing_Allocate: no override for abstract procedure Allocate
  end Nest1;                                  -- POSSIBLE ERROR: [Set1] {1:5;1}


  package Nest2 is
    type Missing_Deallocate is
      new System.Storage_Pools.Root_Storage_Pool with null record;
                                              -- POSSIBLE ERROR: [Set2] {2:5}
     -- type Missing_Deallocate: no override for abstract procedure Deallocate

    procedure Allocate(
      Pool : in out Missing_Deallocate;
      Storage_Address : out System.Address;
      Size_In_Storage_Elements : in System.Storage_Elements.Storage_Count;
      Alignment : in System.Storage_Elements.Storage_Count);

    function Storage_Size( Pool: in Missing_Deallocate )
             return System.Storage_Elements.Storage_Count;

    -- type Missing_Deallocate: no override for abstract procedure Deallocate
  end Nest2;                                  -- POSSIBLE ERROR: [Set2] {1:5;1}


  package Nest3 is
    type Missing_Storage_Size is
      new System.Storage_Pools.Root_Storage_Pool with null record;
                                              -- POSSIBLE ERROR: [Set3] {2:5}
    -- type Missing_Storage_Size: no override for abstract procedure
    -- Storage_Size

    procedure Allocate(
      Pool : in out Missing_Storage_Size;
      Storage_Address : out System.Address;
      Size_In_Storage_Elements : in System.Storage_Elements.Storage_Count;
      Alignment : in System.Storage_Elements.Storage_Count);

    procedure Deallocate(
      Pool : in out Missing_Storage_Size;
      Storage_Address : in System.Address;
      Size_In_Storage_Elements : in System.Storage_Elements.Storage_Count;
      Alignment : in System.Storage_Elements.Storage_Count);

    -- type Missing_Storage_Size: no override for abstract procedure
    -- Storage_Size
  end Nest3;                                  -- POSSIBLE ERROR: [Set3] {1:5;1}

end BDB0A01;

-- package body BDB0A01 not required for this test.
