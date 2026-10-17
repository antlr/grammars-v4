-- Creating an Administrative User
CREATE USER ivanov
FROM hr AS
PERMANENT = 1000000,
PASSWORD = (EXPIRE = 0),
SPOOL = 1200000,
FALLBACK PROTECTION,
DUAL AFTER JOURNAL,
DEFAULT JOURNAL TABLE = hr.journals,
DEFAULT DATABASE = hr,
STARTUP = 'EXEC setpf;',
ACCOUNT = '$mhr','$hhr';

-- Using Constant Expression to Specify PERM, SPOOL, and TEMPORARY Space for User
CREATE USER space_expr AS
DEFAULT DATABASE = it_dev,
PASSWORD = (EXPIRE = 0),
PERM = 2000000*(HASHAMP()+1),
SPOOL = 2000000*(HASHAMP()+1),
TEMPORARY = 2000000*(HASHAMP()+1);

-- Creating User with Space Skew Limit
CREATE USER user1 AS
PASSWORD = (EXPIRE = 0),
PERM = 1e9 SKEW = 10 PERCENT,
TEMPORARY = 1e9 SKEW = 20 PERCENT,
SPOOL = 1e9 SKEW = 20 PERCENT;

-- Creating User Using TIME ZONE='time_zone_string'
CREATE USER user_name
FROM r_n_d AS
PERMANENT = 1000000,
PASSWORD = (EXPIRE = 0),
SPOOL = 1200000,
FALLBACK PROTECTION,
DUAL AFTER JOURNAL,
DEFAULT JOURNAL TABLE = r_n_d.journals,
DEFAULT DATABASE = r_n_d,
STARTUP = 'EXEC setpf;',
ACCOUNT = '$mr_n_d','$hr_n_d',
TIME ZONE = 'Europe/Berlin';

-- Create User that Includes UDT Transforms
CREATE USER TEST_XFORM AS PERM=4E7
PASSWORD = (EXPIRE = 0)
TRANSFORM (XMLD_STRUCT2 = XMLD_STRUCT2INT, JSON CHARACTER SET LATIN = TD_JSON_VARCHAR, JSON CHARACTER
    SET UNICODE = TD_JSON_VARCHAR);

-- Creating Users With Row-Level Security Constraints
CREATE USER user_name
    AS
    PERMANENT = 1e6,
    PASSWORD = (EXPIRE = 0),
    CONSTRAINT = classfication_level (TopSecret),
    CONSTRAINT = classification_category (Europe);

-- Creating New User With EXPECTED Export Width Table Name
CREATE USER eew
    FROM r_n_d AS
    PERMANENT = 1000000,
    PASSWORD = (EXPIRE = 0),
    SPOOL = 2200000,
    FALLBACK PROTECTION,
    DUAL AFTER JOURNAL,
    DEFAULT JOURNAL TABLE = r_n_d.journals,
    DEFAULT DATABASE = r_n_d,
    STARTUP = 'EXEC setpf;',
    ACCOUNT = '$mr_n_d','$hr_n_d',
    TIME ZONE = 'Africa Egypt',
    EXPORTWIDTH = 'EXPECTED';
