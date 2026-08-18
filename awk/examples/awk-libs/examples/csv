#!/usr/bin/awk -f

# include library. gawk can use AWKPATH so the actual path isn't needed, see
# the man page (since the path is relative, this assumes the lib dir is in
# AWKPATH or the same dir)
@include "csv.awk";

# usage: create_line(array, max [, sep [, qualifier [, quote_type] ] ])
# Generates an output line in quoted CSV format, from the contents of "array"
# "array" is expected to be an indexed array (1-indexed). "max" is the highest
# index to be used. "sep", if provided, is the field separator. If it is more
# than one character, the first character in the string is used. By default,
# it is a comma. "qualifier", if provided, is the quote character. Like "sep",
# it is one character. The default value is `"'. "quote_type", if provided, is
# used to determine how the output fields are quoted. Valid values are given
# below. For example, the array: a[1]="foo"; a[2]="bar,quux"; a[3]="blah\"baz"
# when called with create_line(a, 3), will return: "foo","bar,quux","blah""baz"
# note: expects a non-sparse array. empty or unset values will become
# empty fields
# Valid values for "quote_type":
#   "t": Quote all strings, do not quote numbers. This is the default
#   "a": Quote all fields
#   "m": Only quote fields with commas or quote characters in them
BEGIN {
  print "create_line:";
  print "";

  # populate array
  a[1] = "foo"; a[2] = "with,comma"; a[3] = ""; a[4] = "with\"quote";

  # print array in CSV format
  print create_line(a, 4);

  print "";
  print "";
}

# usage: qsplit(string, array [, sep [, qualifier] ])
# a version of split() designed for CSV-like data. splits "string" on "sep"
# (,) if not provided, into array[1], array[2], ... array[n]. returns "n", or
# "-1 * n" if the line is incomplete (it has an uneven number of quotes). both
# "sep" and "qualifier" will use the first character in the provided string.
# uses "qualifier" (" if not provided) and ignores "sep" within quoted fields.
# doubled qualifiers are considered escaped, and a single qualifier character
# is used in its place. for example, foo,"bar,baz""blah",quux will be split as
# such: array[1] = "foo"; array[2] = "bar,baz\"blah"; array[3] = "quux";
BEGIN {
  print "qsplit:";
  print "";

  # populate initial string
  str = "\"foo\",\"with,comma\",\"\",\"with\"\"quote\"";

  print "initial string: ";
  print str;
  print "";

  # split string into array
  len = qsplit(str, b);
  # get the absolute value of the length (could also be done with abs() in
  # math.awk)
  len = len < 0 ? -len : len;

  print "one element per line:"

  # print one element per line
  for (i=1; i<=len; i++) {
    print b[i];
  }
}
