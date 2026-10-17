#!/usr/bin/awk -f

# include library. gawk can use AWKPATH so the actual path isn't needed, see
# the man page (since the path is relative, this assumes the lib dir is in
# AWKPATH or the same dir)
@include "times.awk";

# usage: month_to_num(month)
# converts human readable month to the decimal representation
# returns the number, -1 if the month doesn't exist
BEGIN {
  print "month_to_num(\"January\"): " month_to_num("January");
  print "month_to_num(\"Jan\"): " month_to_num("Jan");

  print "";
}

# usage: day_to_num(day)
# converts human readable day to the decimal representation
# returns the number, -1 if the day doesn't exist
# like date +%w, sunday is 0
BEGIN {
  print "day_to_num(\"Monday\"): " day_to_num("Monday");
  print "day_to_num(\"Mon\"): " day_to_num("Mon");

  print "";
}

# usage: hr_to_sec(timestamp)
# converts HH:MM:SS or MM:SS to seconds
# returns -1 if invalid format
BEGIN {
  print "hr_to_sec(\"00:05:03\"): " hr_to_sec("00:05:03");

  print "";
}

# usage: sec_to_hr(seconds)
# converts seconds to HH:MM:SS
BEGIN {
  print "sec_to_hr(500): " sec_to_hr(500);

  print "";
}

# usage: ms_to_hr(milliseconds)
# converts milliseconds to a "time(1)"-similar human readable format, such
# as 1m4.356s
BEGIN {
  print "ms_to_hr(116529): " ms_to_hr(116529);

  print "";
}

# usage: add_day_suff(day_of_month)
# prepends the appropriate suffix to "day_of_month". for example,
# add_day_suff(1) will return "1st", and add_day_suff(22) will return "22nd"
# returns -1 if "day_of_month" is not a positive integer
BEGIN {
  print "adding suffixes for days 1-31:";
  for (d=1; d<=31; d++) {
    printf("add_day_suff(%s) -> %s\n", d, add_day_suff(d));
  }
}
