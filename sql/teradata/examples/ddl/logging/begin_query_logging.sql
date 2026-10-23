-- Logging All Users for All Accounts
BEGIN QUERY LOGGING ON ALL;

-- Logging Up To 1,000 SQL Text Characters in DBQLogTbl
BEGIN QUERY LOGGING WITH STEPINFO, OBJECTS LIMIT SQLTEXT=1000 ON user_1, user_2;

-- Handling Short Queries
BEGIN QUERY LOGGING LIMIT THRESHOLD=4 AND SQLTEXT=100 ON user_1, user_2;

-- Logging Requests by Summary Completion Time in Full Second Intervals
BEGIN QUERY LOGGING LIMIT SUMMARY=1,5,10 ELAPSEDSEC ON user_1;

-- Disabling Query Logging for MultiLoad Jobs
BEGIN QUERY LOGGING WITH NONE ON APPLNAME ='MULTLOAD';

-- Logging Requests Except for a Single User and Account
BEGIN QUERY LOGGING WITH SQL ON ALL;
BEGIN QUERY LOGGING WITH NONE ON myuser2 ACCOUNT='marketing';

-- Logging Plans as XML Text
BEGIN QUERY LOGGING WITH XMLPLAN LIMIT SQLTEXT=0 on u1;
