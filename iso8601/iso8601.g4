// Copyright (C) 2020 by Student Main
//
// Permission to use, copy, modify, and/or distribute this software for any
// purpose with or without fee is hereby granted.
//
// THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES WITH
// REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY
// AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY SPECIAL, DIRECT,
// INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM
// LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR
// OTHER TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR
// PERFORMANCE OF THIS SOFTWARE.

// ISO 8601 all-in-one grammar file
// choose an entry rule on your demand
// search 'ENTRYRULE' to find all intended entry points

// reference document:
// https://www.loc.gov/standards/datetime/iso-tc154-wg5_n0038_iso_wd_8601-1_2016-02-16.pdf

grammar iso8601;

// lexer
Newline: [\r\n] -> channel(HIDDEN);
T: [Tt];
Z: [Zz];
W: [Ww];
P: [Pp];
Y: [Yy];
M: [Mm];
D: [Dd];
H: [Hh];
S: [Ss];
R: [Rr];
Digit: [0-9];
Fraction: [,.] Digit+;

// number
int_: Digit+;
dec: Digit+ Fraction?;
int2: Digit Digit;
int3: Digit Digit Digit;
int4: Digit Digit Digit Digit;
sint2p: ('+'|'-') Digit Digit+;
sint4p: ('+'|'-') Digit Digit Digit Digit+;
dec2: Digit Digit Fraction?;

// datetime element
century
    : int2      # CompleteCentury
    | sint2p    # ExpandedCentury
    ;
year
    : int4      # CompleteYear
    | sint4p    # ExpandedYear
    ;
month: int2;
day: int2;
ordinalDay: int3;
week: int2;
weekDay: Digit;
hour: int2;
minute: int2;
second: int2;
hourFraction: dec2;
minuteFraction: dec2;
secondFraction: dec2;

// ENTRYRULE 1926-08-17 4.1.2.2 & 4.1.2.4 a
calendarDate: calendarDateBasic | calendarDateExtended;
calendarDateBasic: year month day;
calendarDateExtended: year '-' month '-' day;

// ENTRYRULE 1926-08 4.1.2.3 a & 4.1.2.4 b
specificMonthCalendarDate: year '-' month;
// ENTRYRULE 1926 4.1.2.3 b & 4.1.2.4 c
specificYearCalendarDate: year;
// ENTRYRULE 19 (it means 20 century here) 4.1.2.3 c & 4.1.2.4 d
specificCenturyCalendarDate: century;

// ENTRYRULE 1926-229 4.1.3.2 & 4.1.3.3
ordinalDate: ordinalDateBasic | ordinalDateExtended;
ordinalDateBasic: year ordinalDay;
ordinalDateExtended: year '-' ordinalDay;

// ENTRYRULE 1926-W33-2 4.1.4.2 & 4.1.4.4 a
weekDate: weekDateBasic | weekDateExtended;
weekDateBasic: year W week weekDay;
weekDateExtended: year '-' W week '-' weekDay;

// ENTRYRULE 1926-W33 4.1.4.3 & 4.1.4.4 b
specificWeekWeekDate: specificWeekWeekDateBasic | specificWeekWeekDateExtended;
specificWeekWeekDateBasic: year W week;
specificWeekWeekDateExtended: year '-' W week;

// ENTRYRULE (any date precise to day)
datePrecise: datePreciseBasic | datePreciseExtended;
datePreciseBasic
    : calendarDateBasic
    | ordinalDateBasic
    | weekDateBasic
    ;
datePreciseExtended
    : calendarDateExtended
    | ordinalDateExtended
    | weekDateExtended
    ;

// ENTRYRULE (any date format)
date: dateBasic | dateExtended;
dateBasic
    : datePreciseBasic
    | specificMonthCalendarDate
    | specificYearCalendarDate
    | specificCenturyCalendarDate
    | specificWeekWeekDateBasic
    ;
dateExtended
    : datePreciseExtended
    | specificMonthCalendarDate
    | specificYearCalendarDate
    | specificCenturyCalendarDate
    | specificWeekWeekDateExtended
    ;

// ENTRYRULE 12:34:56.0721 4.2.2.2 & 4.2.2.4 a
localTimePrecise: localTimePreciseBasic | localTimePreciseExtended;
localTimePreciseBasic: hour minute secondFraction;
localTimePreciseExtended: hour ':' minute ':' secondFraction;

// ENTRYRULE 12:34.5 4.2.2.3 a & 4.2.2.4 b
specificMinuteLocalTime: specificMinuteLocalTimeBasic | specificMinuteLocalTimeExtended;
specificMinuteLocalTimeBasic: hour minuteFraction;
specificMinuteLocalTimeExtended: hour ':' minuteFraction;

// ENTRYRULE 12.5 (12:30:00) 4.2.2.3.b & 4.2.2.4 c
specificHourLocalTime: hourFraction;

// ENTRYRULE (time without T and timezone)
localTime: localTimeBasic | localTimeExtended;
localTimeBasic
    : localTimePreciseBasic
    | specificMinuteLocalTimeBasic
    | specificHourLocalTime
    ;
localTimeExtended
     : localTimePreciseExtended
     | specificMinuteLocalTimeExtended
     | specificHourLocalTime
     ;

// 4.2.4
timeZoneUtc: Z;

// ENTRYRULE +08:00 4.2.5.1
timeZone: timeZoneBasic | timeZoneExtended;
timeZoneBasic: (('+'|'-') hour minute?)|timeZoneUtc;
timeZoneExtended: (('+'|'-') hour (':' minute)?)|timeZoneUtc;

// ENTRYRULE (time with timezone without T) 4.2.5.2
localTimeAndTimeZone: localTimeAndTimeZoneBasic | localTimeAndTimeZoneExtended;
localTimeAndTimeZoneBasic: localTimeBasic timeZoneBasic;
localTimeAndTimeZoneExtended: localTimeExtended timeZoneExtended;

// ENTRYRULE (any time format)
time: T? localTime timeZone?;

// ENTRYRULE 1957-10-05T01:28:34+06:00 4.3.2
datetimePrecise: datetimePreciseBasic | datetimePreciseExtended;
datetimePreciseBasic: calendarDateBasic T localTimePreciseBasic timeZoneBasic?;
datetimePreciseExtended: calendarDateExtended T localTimePreciseExtended timeZoneExtended?;

// ENTRYRULE (any datetime format)4.3.3
datetime: datetimeBasic | datetimeExtended;
datetimeBasic: datePreciseBasic T localTimeBasic timeZoneBasic?;
datetimeExtended: datePreciseExtended T localTimeExtended timeZoneExtended?;

// ENTRYRULE (any interval format) 4.4.3.2 & 4.4.4.2.1
interval: intervalT | intervalYMD;
intervalBasic: intervalT | intervalYMDBasic;
intervalExtended: intervalT | intervalYMDExtended;
// ENTRYRULE P1Y2M3DT4H5M6.789S
intervalT
    : P (int_ Y)? (int_ M)? (int_ D)? T (int_ H)? (int_ M)? dec S
    | P (int_ Y)? (int_ M)? (int_ D)? T (int_ H)? dec M
    | P (int_ Y)? (int_ M)? (int_ D)? T dec H
    | P (int_ Y)? (int_ M)? dec D
    | P (int_ Y)? dec M
    | P dec Y
    | P dec W
    ;

monthDateBasic
    : month day
    ;
monthDateExtended
    : month '-' day
    ;

intervalYMDTimeBasic
    : monthDateBasic
    | day
    | datetimeBasic
    | dateBasic
    | localTimeBasic
    ;
intervalYMDTimeExtended
    : monthDateExtended
    | day
    | datetimeExtended
    | dateExtended
    | localTimeExtended
    | monthDateExtended
    ;

// ENTRYRULE P0001-02-03T00:00:00 4.4.4.2.2
intervalYMD: intervalYMDBasic | intervalYMDExtended;
intervalYMDBasic: P intervalYMDTimeBasic;
intervalYMDExtended: P intervalYMDTimeExtended;

// ENTRYRULE 1234-05-06T00:07:08/1234-05-06T00:07:09 4.4.4.1
timeBeginEnd: timeBeginEndBasic | timeBeginEndExtended;
timeBeginEndBasic: intervalYMDTimeBasic '/' intervalYMDTimeBasic;
timeBeginEndExtended: intervalYMDTimeExtended '/' intervalYMDTimeExtended;

// ENTRYRULE 1234-05-06T00:07:08/P1M 4.4.4.3
timeBeginInterval: timeBeginIntervalBasic|timeBeginIntervalExtended;
timeBeginIntervalBasic: datetimeBasic '/' intervalBasic;
timeBeginIntervalExtended: datetimeExtended '/' intervalExtended;

// ENTRYRULE P138Y/1234-05-06T00:07:09 4.4.4.4
timeIntervalEnd: timeIntervalEndBasic|timeIntervalEndExtended;
timeIntervalEndBasic: intervalBasic '/' datetimeBasic;
timeIntervalEndExtended: intervalExtended '/' datetimeExtended;

// ENTRYRULE (any duration)
duration: durationBasic | durationExtended;
durationBasic
    : timeBeginEndBasic
    | timeBeginIntervalBasic
    | timeIntervalEndBasic
    | intervalBasic
    ;
durationExtended
    : timeBeginEndExtended
    | timeBeginIntervalExtended
    | timeIntervalEndExtended
    | intervalExtended
    ;

recurringCount: R int_?;
// ENTRYRULE R12/1900-01-01T00:00:00/P4294967296S 4.5.3
recurring: recurringBasic | recurringExtended;
recurringBasic: recurringCount '/' durationBasic;
recurringExtended: recurringCount '/' durationExtended;

// try everything
// if you just want parse 1926-08-17T12:34:56+08:00
// use dateTimePrecise instead
// ENTRYRULE (everything)
iso
    :
    // put time before date, to mitigate confusion between century and hour
    ( time
    | date
    | datetime
    | duration
    | recurring
    ) EOF;
