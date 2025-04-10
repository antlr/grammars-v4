-- Modifying Permanent Space Allocation for User
MODIFY USER ivanov AS
    PERMANENT = 6000000 BYTES;

-- Modifying PERM, SPOOL, and TEMPORARY Space Using Constant Expression
MODIFY USER sls120639 AS
    DEFAULT DATABASE = it_dev,
    PASSWORD = (EXPIRE = 0),
    PERM = 3000000*(HASHAMP()+1),
    SPOOL = 3000000*(HASHAMP()+1),
    TEMPORARY = 3000000*(HASHAMP()+1);

-- Modifying User to Add Permanent Space Skew Limit
MODIFY USER user1 AS
    PASSWORD = (EXPIRE = 0),
    PERM = 1e9 SKEW = 10 PERCENT;

-- Modifying Spool Allocation, Startup String, and Default Database
MODIFY USER marks AS
    SPOOL = 1500000,
    STARTUP = 'EXEC paystat;',
    DEFAULT DATABASE = payroll;

-- Modifying User to Add Temporary Space Skew Limit
MODIFY USER user1 AS
    PASSWORD = (EXPIRE = 0),
    PERM = 1e9,
    TEMPORARY = 1e9 SKEW = 20 PERCENT;

-- Modifying User to Add Spool Space Skew Limit
MODIFY USER user1 AS
    PASSWORD = (EXPIRE = 0),
    PERM = 1e9,
    SPOOL = 1e9 SKEW = 20 PERCENT;

-- Modifying Default Database for User
MODIFY USER chin AS
    DEFAULT DATABASE = personnel;

-- Modifying Default Journal Table for User
MODIFY USER jones AS
    DROP DEFAULT JOURNAL TABLE;

MODIFY USER jones AS
    DEFAULT JOURNAL TABLE = jrnl1;

-- Modifying Time Zone Displacement with TIME ZONE LOCAL Option
MODIFY USER user_name AS
    TIME ZONE = LOCAL;

-- Modifying Time Zone Displacement with TIME ZONE time_zone_string Option
MODIFY USER pa AS
    TIME ZONE = 'Europe/Berlin';

-- Assigning Profile to User with PROFILE Option
MODIFY USER ivanov AS
    PROFILE = hr;

-- Adding UDT Transform Groups to User
MODIFY USER TEST_XFORM AS
TRANSFORM (XMLD_STRUCT2 = XMLD_STRUCT2INT,
    JSON CHARACTER SET LATIN = TD_JSON_VARCHAR, JSON CHARACTER SET UNICODE = TD_JSON_VARCHAR);

-- Removing UDT Transform Groups from User
MODIFY USER TEST_XFORM AS TRANSFORM ();

-- Adding or Dropping Security Constraint Assignments in MODIFY USER Statement
MODIFY USER
    petrov AS
    CONSTRAINT = Classification_Level (TopSecret, Unclassified DEFAULT),
    CONSTRAINT = Classification_Country (NULL),
    CONSTRAINT = Classification_Job (Analyst);
