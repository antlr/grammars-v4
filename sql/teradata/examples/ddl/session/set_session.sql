-- Setting the Account to Change Resource Charges for a Session
SET SESSION ACCOUNT='$M0+FIN1&S&D&H' FOR SESSION;

-- Changing the Default Session Calendar Using SET SESSION CALENDAR
SET SESSION CALENDAR=ISO;

SET SESSION CALENDAR=COMPATIBLE;

-- Enabling Unicode Pass Through for the Session
SET SESSION CHARACTER SET UNICODE PASS THROUGH ON;

-- Setting a Session to Use the ASCII Collation Sequence
SET SESSION COLLATION ASCII;

-- Changing the Row-Level Security Level for a Session
SET SESSION CONSTRAINT = classification_level (secret);

-- Change default database for current session to 'accounting'
SET SESSION DATABASE accounting;

-- Changing the Session Dateform to ANSIDATE
SET SESSION DATEFORM = ANSIDATE;

-- Enabling Debugging for the Session
SET SESSION DEBUG FUNCTION function_name ON;

-- Return an error for pre-16.0 dot notation query results
SET SESSION DOT NOTATION DEFAULT ON ERROR;

-- Enabling Concurrent Isolated Loading for the Session
SET SESSION FOR ISOLATED LOADING;

-- Enable function trace processing for the current session, writing the trace data into the GTTT udf_diagnostics
SET SESSION FUNCTION TRACE USING 'diag,3'
FOR TABLE udf_diagnostics;

-- Disabling JSON Data Validation
SET SESSION JSON IGNORE ERRORS ON;

-- Setting the Database UIF Search Path
SET SESSION SEARCHUIFDBPATH = DB1, DB2;

-- Setting the Default Session Isolation Level To READ UNCOMMITTED
SET SESSION CHARACTERISTICS AS TRANSACTION ISOLATION LEVEL READ UNCOMMITTED;

-- Setting the Time Zone to LOCAL
SET TIME ZONE LOCAL;

-- Setting the Time Zone Using a Simple INTERVAL HOUR TO MONTH Time Zone Constant Expression Displacement String
SET TIME ZONE INTERVAL '08:00' HOUR TO MINUTE;

-- Setting the Time Zone Using a Simple Constant Expression
SET TIME ZONE +1;

-- Search function bitor first in functions database, then in syslib, and finally in td_sysfnlib
SET SESSION UDFSEARCHPATH = functions, SYSLIB, TD_SYSFNLIB FOR FUNCTION = bitor;
