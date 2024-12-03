-- Change Permanent Space Allocation
MODIFY DATABASE hr AS
    PERMANENT = 6000000 BYTES;

-- Use Constant Expression to Modify PERM, SPOOL, and TEMPORARY Database Spaces
MODIFY DATABASE dev AS
    PERM = 3000000*(HASHAMP()+1),
    SPOOL = 3000000*(HASHAMP()+1),
    TEMPORARY = 3000000*(HASHAMP()+1);

-- Add Space Skew Limit to Database
MODIFY DATABASE space_skew AS
    PERM = 1e9 SKEW = 10 PERCENT,
    TEMPORARY = 1e9 SKEW = 20 PERCENT,
    SPOOL = 1e9 SKEW = 20 PERCENT;

-- Change Fallback Protection and Space Allocation
MODIFY DATABASE finance AS
    PERM = 75000000,
    SPOOL = 150000000,
    NO FALLBACK,
    DROP DEFAULT JOURNAL TABLE = finance.journals;

-- Change Default Journal Table
MODIFY DATABASE Personnel AS
    DEFAULT JOURNAL TABLE = Jrnl1;

-- Drop Default Journal Table
MODIFY DATABASE Personnel AS
    DROP DEFAULT JOURNAL TABLE = Jrnl1;

-- Define New Default Journal Table
MODIFY DATABASE Personnel AS
    DEFAULT JOURNAL TABLE = Jrnl1;
