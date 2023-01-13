--
-- Test assorted system views
--
-- This test is mainly meant to provide some code coverage for the
-- set-returning functions that underlie certain system views.
-- The output of most of these functions is very environment-dependent,
-- so our ability to test with fixed expected output is pretty limited;
-- but even a trivial check of count(*) will exercise the normal code path
-- through the SRF.

select count(*) >= 0 as ok from pg_available_extension_versions;

select count(*) >= 0 as ok from pg_available_extensions;

-- At introduction, pg_config had 23 entries; it may grow
select count(*) > 20 as ok from pg_config;

-- We expect no cursors in this test; see also portals.sql
select count(*) = 0 as ok from pg_cursors;

select count(*) >= 0 as ok from pg_file_settings;

-- There will surely be at least one rule
select count(*) > 0 as ok from pg_hba_file_rules;

-- There will surely be at least one active lock
select count(*) > 0 as ok from pg_locks;

-- We expect no prepared statements in this test; see also prepare.sql
select count(*) = 0 as ok from pg_prepared_statements;

-- See also prepared_xacts.sql
select count(*) >= 0 as ok from pg_prepared_xacts;

-- This is to record the prevailing planner enable_foo settings during
-- a regression test run.
select name, setting from pg_settings where name like 'enable%';

-- Test that the pg_timezone_names and pg_timezone_abbrevs views are
-- more-or-less working.  We can't test their contents in any great detail
-- without the outputs changing anytime IANA updates the underlying data,
-- but it seems reasonable to expect at least one entry per major meridian.
-- (At the time of writing, the actual counts are around 38 because of
-- zones using fractional GMT offsets, so this is a pretty loose test.)
select count(distinct utc_offset) >= 24 as ok from pg_timezone_names;
select count(distinct utc_offset) >= 24 as ok from pg_timezone_abbrevs;
-- Let's check the non-default timezone abbreviation sets, too
set timezone_abbreviations = 'Australia';
select count(distinct utc_offset) >= 24 as ok from pg_timezone_abbrevs;
set timezone_abbreviations = 'India';
select count(distinct utc_offset) >= 24 as ok from pg_timezone_abbrevs;
