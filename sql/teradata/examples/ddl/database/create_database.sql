-- Creating a New Database
CREATE DATABASE finance FROM sysdba
    AS PERMANENT = 60000000,
    SPOOL = 120000000,
    FALLBACK PROTECTION,
    AFTER JOURNAL,
    BEFORE JOURNAL,
    DEFAULT JOURNAL TABLE = finance.journals,
    ACCOUNT = 'ACC';

-- Using a Constant Expression to Specify the PERM, SPOOL, and TEMPORARY Space for a Database
CREATE DATABASE sales AS
    PERM = 2000000*(HASHAMP() + 1),
    SPOOL = 2000000*(HASHAMP() + 1),
    TEMPORARY = 2000000*(HASHAMP() + 1);

-- Creating a Database with a Space Skew
CREATE DATABASE perm_skew AS
    PERM = 1e9 SKEW = 10 PERCENT,
    SPOOL = 1e9 SKEW = 20 PERCENT,
    TEMPORARY = 1e9 SKEW = 20 PERCENT;

-- Creating a Database with Fallback and Journaling
CREATE DATABASE hr FROM adm
    AS PERMANENT = 8000000 BYTES,
    FALLBACK,
    BEFORE JOURNAL,
    DUAL AFTER JOURNAL,
    DEFAULT JOURNAL TABLE = hr.jrn;
