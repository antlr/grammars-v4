-- Using the Default TDQCD Database to Capture and Store a Query Plan in XML Format
BEGIN QUERY CAPTURE AS workload wl_2;

-- Capturing a Mix of DML and DDL Requests in XML Format Using BEGIN QUERY CAPTURE
BEGIN QUERY CAPTURE
WITH VERBOSE, DETAILED STATSUSAGE
INTO user_1_QCD
AS WORKLOAD wl_1;
