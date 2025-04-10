-- Creating Profile that Defines Default Database
CREATE PROFILE human_resources AS
    DEFAULT DATABASE = personnel;

-- Using Constant Expression to Specify SPOOL Space for Profile
CREATE PROFILE research_and_development AS
    DEFAULT DATABASE = it_dev,
    PASSWORD = (EXPIRE = 0),
    SPOOL = 2000000*(HASHAMP()+1);

-- Creating Profile that Defines User-Level Password Security Attribute
CREATE PROFILE hr AS
DEFAULT DATABASE = personnel,
PASSWORD =
    (EXPIRE = 90,
    MAXLOGONATTEMPTS = 10,
    LOCKEDUSEREXPIRE = -1);

-- Set Profile Query Band Pairs as Default
CREATE PROFILE testprofile AS
    QUERY_BAND = 'IMPORTANCE=high;' (DEFAULT);

-- Creating Profile with System Default Query Band Value and Query Band Value to Ignore
CREATE PROFILE salesprofile AS
    QUERY_BAND = 'GROUP=WestCoast;' (DEFAULT),
    IGNORE QUERY_BAND VALUES = 'TVSTemperature=HOT;';

-- Adding UDT Transform Groups to Profile
CREATE PROFILE DR_PROF AS TRANSFORM (XMLD_STRUCT1 = XMLD_STRUCT1INT, JSON
    CHARACTER SET LATIN = TD_JSON_VARCHAR);

-- Assigning Row-Level Security Constraint Classifications to Profile
CREATE PROFILE profile_name AS
    CONSTRAINT = Classification_Level (Secret, Unclassified DEFAULT),
    CONSTRAINT = Classification_Country (US, UK, GER);
