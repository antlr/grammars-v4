#!/usr/bin/awk -f

# include library. gawk can use AWKPATH so the actual path isn't needed, see
# the man page (since the path is relative, this assumes the lib dir is in
# AWKPATH or the same dir)
@include "strings.awk";

# usage: center(string [, width])
# returns "string" centered based on "width". if "width" is not provided (or 
# is 0), uses the width of the terminal, or 80 if standard output is not open
# on a terminal.
# note: does not check the length of the string. if it's wider than the
# terminal, it will not center lines other than the first. for best results,
# combine with fold().
BEGIN {
  print center("this string is centered");

  print "";
}

# usage: fold(string, sep [, width])
# returns "string", wrapped, with lines broken on "sep" to "width" columns.
# "sep" is a list of characters to break at, similar to IFS in a POSIX shell.
# if "sep" is empty, wraps at exactly "width" characters. if "width" is not
# provided (or is 0), uses the width of the terminal, or 80 if standard output
# is not open on a terminal.
# note: currently, tabs are squeezed to a single space. this will be fixed
BEGIN {
  # folds the alphabet on vowels, to 12 and 15 characters
  alphabet = "ABCDEFGHIJKLMNOPQRSTUVWXYZ";

  print "alphabet, folded to 12 characters on vowels:";
  print "";
  print fold(alphabet, "AEIOU", 12);
  print "";

  print "alphabet, folded to 15 characters on vowels:";
  print "";
  print fold(alphabet, "AEIOU", 15);
  print "";
}

# usage: ssub(ere, repl [, in])
# behaves like sub, except returns the result and doesn't modify the original
BEGIN {
  string = "this is some string";

  print "ssub: ";
  print "";

  print string;
  print ssub("str.*", "replaced &", string);
  print string;

  print "";
}

# usage: sgsub(ere, repl [, in])
# behaves like gsub, except returns the result and doesn't modify the original
BEGIN {
  string = "this is some search string search";

  print "sgsub: ";
  print "";

  print string;
  print sgsub("search", "replace", string);
  print string;

  print "";
}

# usage: lsub(str, repl [, in])
# substites the string "repl" in place of the first instance of "str" in the
# string "in" and returns the result. does not modify the original string.
# if "in" is not provided, uses $0.
BEGIN {
  string = "string with special .* characters";

  print "lsub: ";
  print "";

  print string;
  print lsub(".*", "literal", string);
  print string;

  print "";
}

# usage: glsub(str, repl [, in])
# behaves like lsub, except it replaces all occurances of "str"
BEGIN {
  string = "string with .* special .* characters";

  print "glsub: ";
  print "";

  print string;
  print glsub(".*", "literal", string);
  print string;

  print "";
}

# usage: shell_esc(string)
# returns the string escaped so that it can be used in a shell command
BEGIN {
  file = "some 'filename' with * special characters to be used in system()";

  print "shell escape:"
  print "";
  print "first is normal, second escaped: ";
  print file;
  print shell_esc(file);
  print "";
}

# usage: str_to_arr(string, array)
# converts string to an array, one char per element, 1-indexed
# returns the array length
BEGIN {
  string = "some string";

  print "str_to_arr:";
  print "";

  print "initial string: ";
  print string;
  print "";

  print "one character per line:"
  len = str_to_arr(string, array);

  for (i=1; i<=len; i++) {
    print array[i];
  }

  print "";
}

# usage: extract_range(string, start, stop)
# extracts fields "start" through "stop" from "string", based on FS, with the
# original field separators intact. returns the extracted fields.
BEGIN {
  str = "foo bar   baz\tblah  quux";
  FS = " ";

  printf("extract_range(str, 1, 3): %s\n", extract_range(str, 1, 3));
  printf("extract_range(str, 2, NF): %s\n", extract_range(str, 2, NF));
  printf("extract_range(str, 3, 8): %s\n", extract_range(str, 3, 8));

  print "";
}

# usage: fwidths(width_spec [, string])
# extracts substrings from "string" according to "width_spec" from left to
# right and assigns them to $1, $2, etc. also assigns the NF variable. if
# "string" is not supplied, uses $0. "width_spec" is a space separated list of
# numbers that specify field widths, just like GNU awk's FIELDWIDTHS variable.
# if there is data left over after the last width_spec, adds it to a final
# field. returns the value for NF.
BEGIN {
  str = "1234567890";
  fwidths("3 2 5", str);

  for (i=1; i<=NF; i++) {
    print $i;
  }

  print "";
}

# usage: fwidths_arr(width_spec, array [, string])
# the behavior is the same as that of fwidths(), except that the values are
# assigned to "array", indexed with sequential integers starting with 1.
# returns the length. everything else is described in fwidths() above.
BEGIN {
  str = "1234567890";
  len = fwidths("2 1 4 3", a, str);

  for (i=1; i<=len; i++) {
    print a[i];
  }

  print "";
}

# usage: lsplit(str, arr, sep)
# splits the string "str" into array elements "arr[1]", "arr[2]", .., "arr[n]",
# and returns "n". all elements of "arr" are deleted before the split is
# performed. the separation is done on the literal string "sep".
BEGIN {
  string = "foo.bar.baz"
  sep = ".";
  printf("original: <%s>\nsep: <%s>\n", string, sep);
  
  len = lsplit(string, a, sep);
  print "  after: len = lsplit(string, a, sep)";
  printf("    len: %d\n", len);
  for (i=1; i<=len; i++) {
    printf("    a[%d]: <%s>\n", i, a[i]);
  }

  print "";
}

# usage: ssplit(str, arr, seps [, ere])
# similar to GNU awk 4's "seps" functionality for split(). splits the string
# "str" into the array "arr" and the separators array "seps" on the regular
# expression "ere", and returns the number of fields. the value of "seps[i]"
# is the separator that appeared in front of "arr[i+1]". if "ere" is omitted or
# empty, FS is used instead. if "ere" is a single space, leading whitespace in
# "str" will go into the extra array element "seps[0]" and trailing whitespace
# will go into the extra array element "seps[len]", where "len" is the return
# value.
# note: /regex/ style quoting cannot be used for "ere".
BEGIN {
  string = " one  two   three    ";
  printf("original: <%s>\n", string);

  len = ssplit(string, a, s, " ");
  print "  after: len = ssplit(string, a, s, \" \")";
  printf("    len: %d\n    s[0]: <%s>\n", len, s[0]);
  for (i=1; i<=len; i++) {
    printf("    a[%d]: <%s>, s[%d]: <%s>\n", i, a[i], i, s[i]);
  }
  
  print "";
}

# usage: ends_with(string, substring)
# returns 1 if "strings" ends with "substring", otherwise 0
BEGIN {
  string = "foobar"; s = "bar";
  if (ends_with(string, s)) {
    printf("%s ends with %s\n", string, s);
  } else {
    printf("%s does not end with %s\n", string, s);
  }

  string = "foobarfoo"; s = "bar";
  if (ends_with(string, s)) {
    printf("%s ends with %s\n", string, s);
  } else {
    printf("%s does not end with %s\n", string, s);
  }

  print "";
}

# usage: trim(string)
# returns "string" with leading and trailing whitespace trimmed
BEGIN {
  string = "      whitespace      ";

  print "normal: <" string ">";
  print "after trim(): <" trim(string) ">";

  print "";
}

# usage: rev(string)
# returns "string" backwards
BEGIN {
  string = "forwards";

  print "normal: " string;
  print "after rev(): " rev(string);
}

# usage: max(array [, how ])
# returns the maximum value in "array", 0 if the array is empty, or -1 if an
# error occurs. the optional string "how" controls the comparison mode.
# requires the __mcompare() function.
# valid values for "how" are:
#   "std"
#     use awk's standard rules for comparison. this is the default
#   "str"
#     force comparison as strings
#   "num"
#     force a numeric comparison
BEGIN {
  # populate array with random numbers
  for (i=0; i<10; i++) {
    a[i] = int(rand() * 10);
  }

  # print the max
  print "max: " max(a, "num");
}

# usage: maxi(array [, how ])
# the behavior is the same as that of max(), except that the array indices are
# used, not the array values. everything else is explained in max() above.
BEGIN {
  # populate array with random numbers
  for (i=0; i<10; i++) {
    a[int(rand() * 10)];
  }

  # print the max
  print "max: " maxi(a, "num");
}

# usage: min(array [, how ])
# the behavior is the same as that of max(), except that the minimum value is
# returned instead of the maximum. everything else is explained in max() above.
BEGIN {
  # populate array with random numbers
  for (i=0; i<10; i++) {
    a[i] = int(rand() * 10);
  }

  # print the min
  print "min: " min(a, "num");
}

# usage: mini(array [, how ])
# the behavior is the same as that of min(), except that the array indices are
# used instead of the array values. everything else is explained in min() and
# max() above.
BEGIN {
  # populate array with random numbers
  for (i=0; i<10; i++) {
    a[int(rand() * 10)];
  }

  # print the min
  print "min: " mini(a, "num");
}
