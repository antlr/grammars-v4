--
-- TIMESTAMPTZ
--

CREATE TABLE TIMESTAMPTZ_TBL (d1 timestamp(2) with time zone);

-- Test shorthand input values
-- We can't just "select" the results since they aren't constants; test for
-- equality instead.  We can do that by running the test inside a transaction
-- block, within which the value of 'now' shouldn't change, and so these
-- related values shouldn't either.

BEGIN;

INSERT INTO TIMESTAMPTZ_TBL VALUES ('today');
INSERT INTO TIMESTAMPTZ_TBL VALUES ('yesterday');
INSERT INTO TIMESTAMPTZ_TBL VALUES ('tomorrow');
INSERT INTO TIMESTAMPTZ_TBL VALUES ('tomorrow EST');
INSERT INTO TIMESTAMPTZ_TBL VALUES ('tomorrow zulu');

SELECT count(*) AS One FROM TIMESTAMPTZ_TBL WHERE d1 = timestamp with time zone 'today';
SELECT count(*) AS One FROM TIMESTAMPTZ_TBL WHERE d1 = timestamp with time zone 'tomorrow';
SELECT count(*) AS One FROM TIMESTAMPTZ_TBL WHERE d1 = timestamp with time zone 'yesterday';
SELECT count(*) AS One FROM TIMESTAMPTZ_TBL WHERE d1 = timestamp with time zone 'tomorrow EST';
SELECT count(*) AS One FROM TIMESTAMPTZ_TBL WHERE d1 = timestamp with time zone 'tomorrow zulu';

COMMIT;

DELETE FROM TIMESTAMPTZ_TBL;

-- Verify that 'now' *does* change over a reasonable interval such as 100 msec,
-- and that it doesn't change over the same interval within a transaction block

INSERT INTO TIMESTAMPTZ_TBL VALUES ('now');
SELECT pg_sleep(0.1);

BEGIN;
INSERT INTO TIMESTAMPTZ_TBL VALUES ('now');
SELECT pg_sleep(0.1);
INSERT INTO TIMESTAMPTZ_TBL VALUES ('now');
SELECT pg_sleep(0.1);
SELECT count(*) AS two FROM TIMESTAMPTZ_TBL WHERE d1 = timestamp(2) with time zone 'now';
SELECT count(d1) AS three, count(DISTINCT d1) AS two FROM TIMESTAMPTZ_TBL;
COMMIT;

TRUNCATE TIMESTAMPTZ_TBL;

-- Special values
INSERT INTO TIMESTAMPTZ_TBL VALUES ('-infinity');
INSERT INTO TIMESTAMPTZ_TBL VALUES ('infinity');
INSERT INTO TIMESTAMPTZ_TBL VALUES ('epoch');

-- Postgres v6.0 standard output format
INSERT INTO TIMESTAMPTZ_TBL VALUES ('Mon Feb 10 17:32:01 1997 PST');

-- Variations on Postgres v6.1 standard output format
INSERT INTO TIMESTAMPTZ_TBL VALUES ('Mon Feb 10 17:32:01.000001 1997 PST');
INSERT INTO TIMESTAMPTZ_TBL VALUES ('Mon Feb 10 17:32:01.999999 1997 PST');
INSERT INTO TIMESTAMPTZ_TBL VALUES ('Mon Feb 10 17:32:01.4 1997 PST');
INSERT INTO TIMESTAMPTZ_TBL VALUES ('Mon Feb 10 17:32:01.5 1997 PST');
INSERT INTO TIMESTAMPTZ_TBL VALUES ('Mon Feb 10 17:32:01.6 1997 PST');

-- ISO 8601 format
INSERT INTO TIMESTAMPTZ_TBL VALUES ('1997-01-02');
INSERT INTO TIMESTAMPTZ_TBL VALUES ('1997-01-02 03:04:05');
INSERT INTO TIMESTAMPTZ_TBL VALUES ('1997-02-10 17:32:01-08');
INSERT INTO TIMESTAMPTZ_TBL VALUES ('1997-02-10 17:32:01-0800');
INSERT INTO TIMESTAMPTZ_TBL VALUES ('1997-02-10 17:32:01 -08:00');
INSERT INTO TIMESTAMPTZ_TBL VALUES ('19970210 173201 -0800');
INSERT INTO TIMESTAMPTZ_TBL VALUES ('1997-06-10 17:32:01 -07:00');
INSERT INTO TIMESTAMPTZ_TBL VALUES ('2001-09-22T18:19:20');

-- POSIX format (note that the timezone abbrev is just decoration here)
INSERT INTO TIMESTAMPTZ_TBL VALUES ('2000-03-15 08:14:01 GMT+8');
INSERT INTO TIMESTAMPTZ_TBL VALUES ('2000-03-15 13:14:02 GMT-1');
INSERT INTO TIMESTAMPTZ_TBL VALUES ('2000-03-15 12:14:03 GMT-2');
INSERT INTO TIMESTAMPTZ_TBL VALUES ('2000-03-15 03:14:04 PST+8');
INSERT INTO TIMESTAMPTZ_TBL VALUES ('2000-03-15 02:14:05 MST+7:00');

-- Variations for acceptable input formats
INSERT INTO TIMESTAMPTZ_TBL VALUES ('Feb 10 17:32:01 1997 -0800');
INSERT INTO TIMESTAMPTZ_TBL VALUES ('Feb 10 17:32:01 1997');
INSERT INTO TIMESTAMPTZ_TBL VALUES ('Feb 10 5:32PM 1997');
INSERT INTO TIMESTAMPTZ_TBL VALUES ('1997/02/10 17:32:01-0800');
INSERT INTO TIMESTAMPTZ_TBL VALUES ('1997-02-10 17:32:01 PST');
INSERT INTO TIMESTAMPTZ_TBL VALUES ('Feb-10-1997 17:32:01 PST');
INSERT INTO TIMESTAMPTZ_TBL VALUES ('02-10-1997 17:32:01 PST');
INSERT INTO TIMESTAMPTZ_TBL VALUES ('19970210 173201 PST');
set datestyle to ymd;
INSERT INTO TIMESTAMPTZ_TBL VALUES ('97FEB10 5:32:01PM UTC');
INSERT INTO TIMESTAMPTZ_TBL VALUES ('97/02/10 17:32:01 UTC');
reset datestyle;
INSERT INTO TIMESTAMPTZ_TBL VALUES ('1997.041 17:32:01 UTC');

-- timestamps at different timezones
INSERT INTO TIMESTAMPTZ_TBL VALUES ('19970210 173201 America/New_York');
SELECT '19970210 173201' AT TIME ZONE 'America/New_York';
INSERT INTO TIMESTAMPTZ_TBL VALUES ('19970710 173201 America/New_York');
SELECT '19970710 173201' AT TIME ZONE 'America/New_York';
INSERT INTO TIMESTAMPTZ_TBL VALUES ('19970710 173201 America/Does_not_exist');
SELECT '19970710 173201' AT TIME ZONE 'America/Does_not_exist';

-- Daylight saving time for timestamps beyond 32-bit time_t range.
SELECT '20500710 173201 Europe/Helsinki'::timestamptz; -- DST
SELECT '20500110 173201 Europe/Helsinki'::timestamptz; -- non-DST

SELECT '205000-07-10 17:32:01 Europe/Helsinki'::timestamptz; -- DST
SELECT '205000-01-10 17:32:01 Europe/Helsinki'::timestamptz; -- non-DST

-- Check date conversion and date arithmetic
INSERT INTO TIMESTAMPTZ_TBL VALUES ('1997-06-10 18:32:01 PDT');

INSERT INTO TIMESTAMPTZ_TBL VALUES ('Feb 10 17:32:01 1997');
INSERT INTO TIMESTAMPTZ_TBL VALUES ('Feb 11 17:32:01 1997');
INSERT INTO TIMESTAMPTZ_TBL VALUES ('Feb 12 17:32:01 1997');
INSERT INTO TIMESTAMPTZ_TBL VALUES ('Feb 13 17:32:01 1997');
INSERT INTO TIMESTAMPTZ_TBL VALUES ('Feb 14 17:32:01 1997');
INSERT INTO TIMESTAMPTZ_TBL VALUES ('Feb 15 17:32:01 1997');
INSERT INTO TIMESTAMPTZ_TBL VALUES ('Feb 16 17:32:01 1997');

INSERT INTO TIMESTAMPTZ_TBL VALUES ('Feb 16 17:32:01 0097 BC');
INSERT INTO TIMESTAMPTZ_TBL VALUES ('Feb 16 17:32:01 0097');
INSERT INTO TIMESTAMPTZ_TBL VALUES ('Feb 16 17:32:01 0597');
INSERT INTO TIMESTAMPTZ_TBL VALUES ('Feb 16 17:32:01 1097');
INSERT INTO TIMESTAMPTZ_TBL VALUES ('Feb 16 17:32:01 1697');
INSERT INTO TIMESTAMPTZ_TBL VALUES ('Feb 16 17:32:01 1797');
INSERT INTO TIMESTAMPTZ_TBL VALUES ('Feb 16 17:32:01 1897');
INSERT INTO TIMESTAMPTZ_TBL VALUES ('Feb 16 17:32:01 1997');
INSERT INTO TIMESTAMPTZ_TBL VALUES ('Feb 16 17:32:01 2097');

INSERT INTO TIMESTAMPTZ_TBL VALUES ('Feb 28 17:32:01 1996');
INSERT INTO TIMESTAMPTZ_TBL VALUES ('Feb 29 17:32:01 1996');
INSERT INTO TIMESTAMPTZ_TBL VALUES ('Mar 01 17:32:01 1996');
INSERT INTO TIMESTAMPTZ_TBL VALUES ('Dec 30 17:32:01 1996');
INSERT INTO TIMESTAMPTZ_TBL VALUES ('Dec 31 17:32:01 1996');
INSERT INTO TIMESTAMPTZ_TBL VALUES ('Jan 01 17:32:01 1997');
INSERT INTO TIMESTAMPTZ_TBL VALUES ('Feb 28 17:32:01 1997');
INSERT INTO TIMESTAMPTZ_TBL VALUES ('Feb 29 17:32:01 1997');
INSERT INTO TIMESTAMPTZ_TBL VALUES ('Mar 01 17:32:01 1997');
INSERT INTO TIMESTAMPTZ_TBL VALUES ('Dec 30 17:32:01 1997');
INSERT INTO TIMESTAMPTZ_TBL VALUES ('Dec 31 17:32:01 1997');
INSERT INTO TIMESTAMPTZ_TBL VALUES ('Dec 31 17:32:01 1999');
INSERT INTO TIMESTAMPTZ_TBL VALUES ('Jan 01 17:32:01 2000');
INSERT INTO TIMESTAMPTZ_TBL VALUES ('Dec 31 17:32:01 2000');
INSERT INTO TIMESTAMPTZ_TBL VALUES ('Jan 01 17:32:01 2001');

-- Currently unsupported syntax and ranges
INSERT INTO TIMESTAMPTZ_TBL VALUES ('Feb 16 17:32:01 -0097');
INSERT INTO TIMESTAMPTZ_TBL VALUES ('Feb 16 17:32:01 5097 BC');

-- Alternative field order that we've historically supported (sort of)
-- with regular and POSIXy timezone specs
SELECT 'Wed Jul 11 10:51:14 America/New_York 2001'::timestamptz;
SELECT 'Wed Jul 11 10:51:14 GMT-4 2001'::timestamptz;
SELECT 'Wed Jul 11 10:51:14 GMT+4 2001'::timestamptz;
SELECT 'Wed Jul 11 10:51:14 PST-03:00 2001'::timestamptz;
SELECT 'Wed Jul 11 10:51:14 PST+03:00 2001'::timestamptz;

SELECT '' AS "64", d1 FROM TIMESTAMPTZ_TBL;

-- Check behavior at the lower boundary of the timestamp range
SELECT '4714-11-24 00:00:00+00 BC'::timestamptz;
SELECT '4714-11-23 16:00:00-08 BC'::timestamptz;
SELECT 'Sun Nov 23 16:00:00 4714 PST BC'::timestamptz;
SELECT '4714-11-23 23:59:59+00 BC'::timestamptz;  -- out of range
-- The upper boundary differs between integer and float timestamps, so no check

-- Demonstrate functions and operators
SELECT '' AS "48", d1 FROM TIMESTAMPTZ_TBL
   WHERE d1 > timestamp with time zone '1997-01-02';

SELECT '' AS "15", d1 FROM TIMESTAMPTZ_TBL
   WHERE d1 < timestamp with time zone '1997-01-02';

SELECT '' AS one, d1 FROM TIMESTAMPTZ_TBL
   WHERE d1 = timestamp with time zone '1997-01-02';

SELECT '' AS "63", d1 FROM TIMESTAMPTZ_TBL
   WHERE d1 != timestamp with time zone '1997-01-02';

SELECT '' AS "16", d1 FROM TIMESTAMPTZ_TBL
   WHERE d1 <= timestamp with time zone '1997-01-02';

SELECT '' AS "49", d1 FROM TIMESTAMPTZ_TBL
   WHERE d1 >= timestamp with time zone '1997-01-02';

SELECT '' AS "54", d1 - timestamp with time zone '1997-01-02' AS diff
   FROM TIMESTAMPTZ_TBL WHERE d1 BETWEEN '1902-01-01' AND '2038-01-01';

SELECT '' AS date_trunc_week, date_trunc( 'week', timestamp with time zone '2004-02-29 15:44:17.71393' ) AS week_trunc;

SELECT '' AS date_trunc_at_tz, date_trunc('day', timestamp with time zone '2001-02-16 20:38:40+00', 'Australia/Sydney') as sydney_trunc;  -- zone name
SELECT '' AS date_trunc_at_tz, date_trunc('day', timestamp with time zone '2001-02-16 20:38:40+00', 'GMT') as gmt_trunc;  -- fixed-offset abbreviation
SELECT '' AS date_trunc_at_tz, date_trunc('day', timestamp with time zone '2001-02-16 20:38:40+00', 'VET') as vet_trunc;  -- variable-offset abbreviation

-- Test casting within a BETWEEN qualifier
SELECT '' AS "54", d1 - timestamp with time zone '1997-01-02' AS diff
  FROM TIMESTAMPTZ_TBL
  WHERE d1 BETWEEN timestamp with time zone '1902-01-01' AND timestamp with time zone '2038-01-01';

-- DATE_PART (timestamptz_part)
SELECT d1 as timestamptz,
   date_part( 'year', d1) AS year, date_part( 'month', d1) AS month,
   date_part( 'day', d1) AS day, date_part( 'hour', d1) AS hour,
   date_part( 'minute', d1) AS minute, date_part( 'second', d1) AS second
   FROM TIMESTAMPTZ_TBL;

SELECT d1 as timestamptz,
   date_part( 'quarter', d1) AS quarter, date_part( 'msec', d1) AS msec,
   date_part( 'usec', d1) AS usec
   FROM TIMESTAMPTZ_TBL;

SELECT d1 as timestamptz,
   date_part( 'isoyear', d1) AS isoyear, date_part( 'week', d1) AS week,
   date_part( 'isodow', d1) AS isodow, date_part( 'dow', d1) AS dow,
   date_part( 'doy', d1) AS doy
   FROM TIMESTAMPTZ_TBL;

SELECT d1 as timestamptz,
   date_part( 'decade', d1) AS decade,
   date_part( 'century', d1) AS century,
   date_part( 'millennium', d1) AS millennium,
   round(date_part( 'julian', d1)) AS julian
   FROM TIMESTAMPTZ_TBL;

SELECT d1 as timestamptz,
   date_part( 'timezone', d1) AS timezone,
   date_part( 'timezone_hour', d1) AS timezone_hour,
   date_part( 'timezone_minute', d1) AS timezone_minute
   FROM TIMESTAMPTZ_TBL;

-- TO_CHAR()
SELECT '' AS to_char_1, to_char(d1, 'DAY Day day DY Dy dy MONTH Month month RM MON Mon mon')
   FROM TIMESTAMPTZ_TBL;

SELECT '' AS to_char_2, to_char(d1, 'FMDAY FMDay FMday FMMONTH FMMonth FMmonth FMRM')
   FROM TIMESTAMPTZ_TBL;

SELECT '' AS to_char_3, to_char(d1, 'Y,YYY YYYY YYY YY Y CC Q MM WW DDD DD D J')
   FROM TIMESTAMPTZ_TBL;

SELECT '' AS to_char_4, to_char(d1, 'FMY,YYY FMYYYY FMYYY FMYY FMY FMCC FMQ FMMM FMWW FMDDD FMDD FMD FMJ')
   FROM TIMESTAMPTZ_TBL;

SELECT '' AS to_char_5, to_char(d1, 'HH HH12 HH24 MI SS SSSS')
   FROM TIMESTAMPTZ_TBL;

SELECT '' AS to_char_6, to_char(d1, E'"HH:MI:SS is" HH:MI:SS "\\"text between quote marks\\""')
   FROM TIMESTAMPTZ_TBL;

SELECT '' AS to_char_7, to_char(d1, 'HH24--text--MI--text--SS')
   FROM TIMESTAMPTZ_TBL;

SELECT '' AS to_char_8, to_char(d1, 'YYYYTH YYYYth Jth')
   FROM TIMESTAMPTZ_TBL;

SELECT '' AS to_char_9, to_char(d1, 'YYYY A.D. YYYY a.d. YYYY bc HH:MI:SS P.M. HH:MI:SS p.m. HH:MI:SS pm')
   FROM TIMESTAMPTZ_TBL;

SELECT '' AS to_char_10, to_char(d1, 'IYYY IYY IY I IW IDDD ID')
   FROM TIMESTAMPTZ_TBL;

SELECT '' AS to_char_11, to_char(d1, 'FMIYYY FMIYY FMIY FMI FMIW FMIDDD FMID')
   FROM TIMESTAMPTZ_TBL;

SELECT '' AS to_char_12, to_char(d, 'FF1 FF2 FF3 FF4 FF5 FF6  ff1 ff2 ff3 ff4 ff5 ff6  MS US')
   FROM (VALUES
       ('2018-11-02 12:34:56'::timestamptz),
       ('2018-11-02 12:34:56.78'),
       ('2018-11-02 12:34:56.78901'),
       ('2018-11-02 12:34:56.78901234')
   ) d(d);

-- Check OF, TZH, TZM with various zone offsets, particularly fractional hours
SET timezone = '00:00';
SELECT to_char(now(), 'OF') as "OF", to_char(now(), 'TZH:TZM') as "TZH:TZM";
SET timezone = '+02:00';
SELECT to_char(now(), 'OF') as "OF", to_char(now(), 'TZH:TZM') as "TZH:TZM";
SET timezone = '-13:00';
SELECT to_char(now(), 'OF') as "OF", to_char(now(), 'TZH:TZM') as "TZH:TZM";
SET timezone = '-00:30';
SELECT to_char(now(), 'OF') as "OF", to_char(now(), 'TZH:TZM') as "TZH:TZM";
SET timezone = '00:30';
SELECT to_char(now(), 'OF') as "OF", to_char(now(), 'TZH:TZM') as "TZH:TZM";
SET timezone = '-04:30';
SELECT to_char(now(), 'OF') as "OF", to_char(now(), 'TZH:TZM') as "TZH:TZM";
SET timezone = '04:30';
SELECT to_char(now(), 'OF') as "OF", to_char(now(), 'TZH:TZM') as "TZH:TZM";
SET timezone = '-04:15';
SELECT to_char(now(), 'OF') as "OF", to_char(now(), 'TZH:TZM') as "TZH:TZM";
SET timezone = '04:15';
SELECT to_char(now(), 'OF') as "OF", to_char(now(), 'TZH:TZM') as "TZH:TZM";
RESET timezone;

CREATE TABLE TIMESTAMPTZ_TST (a int , b timestamptz);

-- Test year field value with len > 4
INSERT INTO TIMESTAMPTZ_TST VALUES(1, 'Sat Mar 12 23:58:48 1000 IST');
INSERT INTO TIMESTAMPTZ_TST VALUES(2, 'Sat Mar 12 23:58:48 10000 IST');
INSERT INTO TIMESTAMPTZ_TST VALUES(3, 'Sat Mar 12 23:58:48 100000 IST');
INSERT INTO TIMESTAMPTZ_TST VALUES(3, '10000 Mar 12 23:58:48 IST');
INSERT INTO TIMESTAMPTZ_TST VALUES(4, '100000312 23:58:48 IST');
INSERT INTO TIMESTAMPTZ_TST VALUES(4, '1000000312 23:58:48 IST');
--Verify data
SELECT * FROM TIMESTAMPTZ_TST ORDER BY a;
--Cleanup
DROP TABLE TIMESTAMPTZ_TST;

-- test timestamptz constructors
set TimeZone to 'America/New_York';

-- numeric timezone
SELECT make_timestamptz(1973, 07, 15, 08, 15, 55.33);
SELECT make_timestamptz(1973, 07, 15, 08, 15, 55.33, '+2');
SELECT make_timestamptz(1973, 07, 15, 08, 15, 55.33, '-2');
WITH tzs (tz) AS (VALUES
    ('+1'), ('+1:'), ('+1:0'), ('+100'), ('+1:00'), ('+01:00'),
    ('+10'), ('+1000'), ('+10:'), ('+10:0'), ('+10:00'), ('+10:00:'),
    ('+10:00:1'), ('+10:00:01'),
    ('+10:00:10'))
     SELECT make_timestamptz(2010, 2, 27, 3, 45, 00, tz), tz FROM tzs;

-- these should fail
SELECT make_timestamptz(1973, 07, 15, 08, 15, 55.33, '2');
SELECT make_timestamptz(2014, 12, 10, 10, 10, 10, '+16');
SELECT make_timestamptz(2014, 12, 10, 10, 10, 10, '-16');

-- should be true
SELECT make_timestamptz(1973, 07, 15, 08, 15, 55.33, '+2') = '1973-07-15 08:15:55.33+02'::timestamptz;

-- full timezone names
SELECT make_timestamptz(2014, 12, 10, 0, 0, 0, 'Europe/Prague') = timestamptz '2014-12-10 00:00:00 Europe/Prague';
SELECT make_timestamptz(2014, 12, 10, 0, 0, 0, 'Europe/Prague') AT TIME ZONE 'UTC';
SELECT make_timestamptz(1846, 12, 10, 0, 0, 0, 'Asia/Manila') AT TIME ZONE 'UTC';
SELECT make_timestamptz(1881, 12, 10, 0, 0, 0, 'Europe/Paris') AT TIME ZONE 'UTC';
SELECT make_timestamptz(1910, 12, 24, 0, 0, 0, 'Nehwon/Lankhmar');

-- abbreviations
SELECT make_timestamptz(2008, 12, 10, 10, 10, 10, 'EST');
SELECT make_timestamptz(2008, 12, 10, 10, 10, 10, 'EDT');
SELECT make_timestamptz(2014, 12, 10, 10, 10, 10, 'PST8PDT');

RESET TimeZone;

--
-- Test behavior with a dynamic (time-varying) timezone abbreviation.
-- These tests rely on the knowledge that MSK (Europe/Moscow standard time)
-- moved forwards in Mar 2011 and backwards again in Oct 2014.
--

SET TimeZone to 'UTC';

SELECT '2011-03-27 00:00:00 Europe/Moscow'::timestamptz;
SELECT '2011-03-27 01:00:00 Europe/Moscow'::timestamptz;
SELECT '2011-03-27 01:59:59 Europe/Moscow'::timestamptz;
SELECT '2011-03-27 02:00:00 Europe/Moscow'::timestamptz;
SELECT '2011-03-27 02:00:01 Europe/Moscow'::timestamptz;
SELECT '2011-03-27 02:59:59 Europe/Moscow'::timestamptz;
SELECT '2011-03-27 03:00:00 Europe/Moscow'::timestamptz;
SELECT '2011-03-27 03:00:01 Europe/Moscow'::timestamptz;
SELECT '2011-03-27 04:00:00 Europe/Moscow'::timestamptz;

SELECT '2011-03-27 00:00:00 MSK'::timestamptz;
SELECT '2011-03-27 01:00:00 MSK'::timestamptz;
SELECT '2011-03-27 01:59:59 MSK'::timestamptz;
SELECT '2011-03-27 02:00:00 MSK'::timestamptz;
SELECT '2011-03-27 02:00:01 MSK'::timestamptz;
SELECT '2011-03-27 02:59:59 MSK'::timestamptz;
SELECT '2011-03-27 03:00:00 MSK'::timestamptz;
SELECT '2011-03-27 03:00:01 MSK'::timestamptz;
SELECT '2011-03-27 04:00:00 MSK'::timestamptz;

SELECT '2014-10-26 00:00:00 Europe/Moscow'::timestamptz;
SELECT '2014-10-26 00:59:59 Europe/Moscow'::timestamptz;
SELECT '2014-10-26 01:00:00 Europe/Moscow'::timestamptz;
SELECT '2014-10-26 01:00:01 Europe/Moscow'::timestamptz;
SELECT '2014-10-26 02:00:00 Europe/Moscow'::timestamptz;

SELECT '2014-10-26 00:00:00 MSK'::timestamptz;
SELECT '2014-10-26 00:59:59 MSK'::timestamptz;
SELECT '2014-10-26 01:00:00 MSK'::timestamptz;
SELECT '2014-10-26 01:00:01 MSK'::timestamptz;
SELECT '2014-10-26 02:00:00 MSK'::timestamptz;

SELECT '2011-03-27 00:00:00'::timestamp AT TIME ZONE 'Europe/Moscow';
SELECT '2011-03-27 01:00:00'::timestamp AT TIME ZONE 'Europe/Moscow';
SELECT '2011-03-27 01:59:59'::timestamp AT TIME ZONE 'Europe/Moscow';
SELECT '2011-03-27 02:00:00'::timestamp AT TIME ZONE 'Europe/Moscow';
SELECT '2011-03-27 02:00:01'::timestamp AT TIME ZONE 'Europe/Moscow';
SELECT '2011-03-27 02:59:59'::timestamp AT TIME ZONE 'Europe/Moscow';
SELECT '2011-03-27 03:00:00'::timestamp AT TIME ZONE 'Europe/Moscow';
SELECT '2011-03-27 03:00:01'::timestamp AT TIME ZONE 'Europe/Moscow';
SELECT '2011-03-27 04:00:00'::timestamp AT TIME ZONE 'Europe/Moscow';

SELECT '2011-03-27 00:00:00'::timestamp AT TIME ZONE 'MSK';
SELECT '2011-03-27 01:00:00'::timestamp AT TIME ZONE 'MSK';
SELECT '2011-03-27 01:59:59'::timestamp AT TIME ZONE 'MSK';
SELECT '2011-03-27 02:00:00'::timestamp AT TIME ZONE 'MSK';
SELECT '2011-03-27 02:00:01'::timestamp AT TIME ZONE 'MSK';
SELECT '2011-03-27 02:59:59'::timestamp AT TIME ZONE 'MSK';
SELECT '2011-03-27 03:00:00'::timestamp AT TIME ZONE 'MSK';
SELECT '2011-03-27 03:00:01'::timestamp AT TIME ZONE 'MSK';
SELECT '2011-03-27 04:00:00'::timestamp AT TIME ZONE 'MSK';

SELECT '2014-10-26 00:00:00'::timestamp AT TIME ZONE 'Europe/Moscow';
SELECT '2014-10-26 00:59:59'::timestamp AT TIME ZONE 'Europe/Moscow';
SELECT '2014-10-26 01:00:00'::timestamp AT TIME ZONE 'Europe/Moscow';
SELECT '2014-10-26 01:00:01'::timestamp AT TIME ZONE 'Europe/Moscow';
SELECT '2014-10-26 02:00:00'::timestamp AT TIME ZONE 'Europe/Moscow';

SELECT '2014-10-26 00:00:00'::timestamp AT TIME ZONE 'MSK';
SELECT '2014-10-26 00:59:59'::timestamp AT TIME ZONE 'MSK';
SELECT '2014-10-26 01:00:00'::timestamp AT TIME ZONE 'MSK';
SELECT '2014-10-26 01:00:01'::timestamp AT TIME ZONE 'MSK';
SELECT '2014-10-26 02:00:00'::timestamp AT TIME ZONE 'MSK';

SELECT make_timestamptz(2014, 10, 26, 0, 0, 0, 'MSK');
SELECT make_timestamptz(2014, 10, 26, 1, 0, 0, 'MSK');

SELECT to_timestamp(         0);          -- 1970-01-01 00:00:00+00
SELECT to_timestamp( 946684800);          -- 2000-01-01 00:00:00+00
SELECT to_timestamp(1262349296.7890123);  -- 2010-01-01 12:34:56.789012+00
-- edge cases
SELECT to_timestamp(-210866803200);       --   4714-11-24 00:00:00+00 BC
-- upper limit varies between integer and float timestamps, so hard to test
-- nonfinite values
SELECT to_timestamp(' Infinity'::float);
SELECT to_timestamp('-Infinity'::float);
SELECT to_timestamp('NaN'::float);


SET TimeZone to 'Europe/Moscow';

SELECT '2011-03-26 21:00:00 UTC'::timestamptz;
SELECT '2011-03-26 22:00:00 UTC'::timestamptz;
SELECT '2011-03-26 22:59:59 UTC'::timestamptz;
SELECT '2011-03-26 23:00:00 UTC'::timestamptz;
SELECT '2011-03-26 23:00:01 UTC'::timestamptz;
SELECT '2011-03-26 23:59:59 UTC'::timestamptz;
SELECT '2011-03-27 00:00:00 UTC'::timestamptz;

SELECT '2014-10-25 21:00:00 UTC'::timestamptz;
SELECT '2014-10-25 21:59:59 UTC'::timestamptz;
SELECT '2014-10-25 22:00:00 UTC'::timestamptz;
SELECT '2014-10-25 22:00:01 UTC'::timestamptz;
SELECT '2014-10-25 23:00:00 UTC'::timestamptz;

RESET TimeZone;

SELECT '2011-03-26 21:00:00 UTC'::timestamptz AT TIME ZONE 'Europe/Moscow';
SELECT '2011-03-26 22:00:00 UTC'::timestamptz AT TIME ZONE 'Europe/Moscow';
SELECT '2011-03-26 22:59:59 UTC'::timestamptz AT TIME ZONE 'Europe/Moscow';
SELECT '2011-03-26 23:00:00 UTC'::timestamptz AT TIME ZONE 'Europe/Moscow';
SELECT '2011-03-26 23:00:01 UTC'::timestamptz AT TIME ZONE 'Europe/Moscow';
SELECT '2011-03-26 23:59:59 UTC'::timestamptz AT TIME ZONE 'Europe/Moscow';
SELECT '2011-03-27 00:00:00 UTC'::timestamptz AT TIME ZONE 'Europe/Moscow';

SELECT '2014-10-25 21:00:00 UTC'::timestamptz AT TIME ZONE 'Europe/Moscow';
SELECT '2014-10-25 21:59:59 UTC'::timestamptz AT TIME ZONE 'Europe/Moscow';
SELECT '2014-10-25 22:00:00 UTC'::timestamptz AT TIME ZONE 'Europe/Moscow';
SELECT '2014-10-25 22:00:01 UTC'::timestamptz AT TIME ZONE 'Europe/Moscow';
SELECT '2014-10-25 23:00:00 UTC'::timestamptz AT TIME ZONE 'Europe/Moscow';

SELECT '2011-03-26 21:00:00 UTC'::timestamptz AT TIME ZONE 'MSK';
SELECT '2011-03-26 22:00:00 UTC'::timestamptz AT TIME ZONE 'MSK';
SELECT '2011-03-26 22:59:59 UTC'::timestamptz AT TIME ZONE 'MSK';
SELECT '2011-03-26 23:00:00 UTC'::timestamptz AT TIME ZONE 'MSK';
SELECT '2011-03-26 23:00:01 UTC'::timestamptz AT TIME ZONE 'MSK';
SELECT '2011-03-26 23:59:59 UTC'::timestamptz AT TIME ZONE 'MSK';
SELECT '2011-03-27 00:00:00 UTC'::timestamptz AT TIME ZONE 'MSK';

SELECT '2014-10-25 21:00:00 UTC'::timestamptz AT TIME ZONE 'MSK';
SELECT '2014-10-25 21:59:59 UTC'::timestamptz AT TIME ZONE 'MSK';
SELECT '2014-10-25 22:00:00 UTC'::timestamptz AT TIME ZONE 'MSK';
SELECT '2014-10-25 22:00:01 UTC'::timestamptz AT TIME ZONE 'MSK';
SELECT '2014-10-25 23:00:00 UTC'::timestamptz AT TIME ZONE 'MSK';

--
-- Test that AT TIME ZONE isn't misoptimized when using an index (bug #14504)
--
create temp table tmptz (f1 timestamptz primary key);
insert into tmptz values ('2017-01-18 00:00+00');
explain (costs off)
select * from tmptz where f1 at time zone 'utc' = '2017-01-18 00:00';
select * from tmptz where f1 at time zone 'utc' = '2017-01-18 00:00';
