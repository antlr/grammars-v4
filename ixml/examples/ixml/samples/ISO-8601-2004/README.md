# ISO 8601, Representation of dates and times

Constructed by Norm Tovey-Walsh, April 2022.

There are two grammars in this directory:

* `iso8601.ixml` accepts an ISO 8601 date, time, datetime, duration,
   interval, or recurrence. I believe that this grammar instantiates
   the rules for ISO 8601:2004. It certainly doesn’t include any of
   the new formats described in ISO 8601:2019-1,2. It also doesn’t
   include the `--MM-DD` format(s) that were described in ISO
   8601:2000 but not in the ISO 8601:2004. (According to
   [Wikipedia](https://en.wikipedia.org/wiki/ISO_8601)).
* `iso8601-list.ixml` accepts a list of ISO 8601 strings on separate lines. This version
   only exists for testing purposes. It _should_ be identical to `iso8601.ixml` except for
   the  additional rules `list-of-iso8601` and `comment`.

The `test-data.txt` file contains a list of valid ISO 8601 strings.
The corresponding visible XML output for those strings is in
`test-data.xml`.

## Dates and times

For the most part, the output is what you’d probably
expect. For example:

```
2022-04-17
```

becomes

```xml
<date>
   <year>2022</year>
   <month>04</month>
   <day>17</day>
</date>
```

and

```
2022-04-17T13:55:24+00:00
```

becomes

```xml
<local-datetime>
   <year>2022</year>
   <month>04</month>
   <day>17</day>
   <hour>13</hour>
   <minute>55</minute>
   <second>24</second>
   <utc-offset>
      <offset-hour direction="+">00</offset-hour>
      <offset-minute>00</offset-minute>
   </utc-offset>
</local-datetime>
```

If a datetime includes a specific offset from UTC, the sign is represented in a `direction` attribute
on the `offset-hour`.

## Durations

There are a few ways to specify durations. The “standard” form is “`P`_n_`Y`_n_`M`_n_`D`_n_`T`_n_`H`_n_`M`_n_`S`”
where all of the years, months, days, etc, are essentially optional, but at least one is required. This grammar
doesn’t attempt to express the constraint that at least one is required. It simply allows them all to be optional.

## Intervals

Things get a little less obvious when it comes to time intervals. As far as I can tell, ISO 8601 allows an interval
to be expressed as a range between two datetimes or as a start- or end-datetime and a duration. For the latter cases, the order of the elements in the `interval` is significant. An interval of twelve hours starting at midnight on 14 April 2022 is:

```xml
<interval>
   <local-datetime>
      <year>2022</year>
      <month>03</month>
      <day>14</day>
      <hour>00</hour>
      <minute>00</minute>
      <second>00</second>
   </local-datetime>
   <duration>
      <duration-hours>12</duration-hours>
   </duration>
</interval>
```

An interval of twelve hours _ending_ at midnight on 15 April 2022 (that is, starting at noon on 14 April) is:

```xml
<interval>
   <duration>
      <duration-hours>12</duration-hours>
   </duration>
   <local-datetime>
      <year>2022</year>
      <month>03</month>
      <day>15</day>
      <hour>00</hour>
      <minute>00</minute>
      <second>00</second>
   </local-datetime>
</interval>
```

## Recurrence 

For the special cases where the number of recurrences is omitted, or
is explicitly `-1`, there is no `repetitions` element in the
`recurrence`. This is intended to signal an unbounded number of recurrences.

## Ambiguity

Two string formats are ambiguous in ISO 8601:

* `20` is either the 20th century or 8pm.
* `2022-04` is either _April, 2022_ or it’s 22 minutes past 8pm in the time zone GMT-04:00.

The time forms can be unambiguously written by preceding them with a ‘T’: `T20` or `T2022-04`
but there’s no unambiguous form for the date forms, as far as I can tell.
