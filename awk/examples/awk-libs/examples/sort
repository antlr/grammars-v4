#!/usr/bin/awk -f

# include library. gawk can use AWKPATH so the actual path isn't needed, see
# the man page (since the path is relative, this assumes the lib dir is in
# AWKPATH or the same dir)
@include "sort.awk";

# usage: qsort(s, d [, how])
# sorts the elements in the array "s" using awk's normal rules for comparing
# values, creating a new sorted array "d" indexed with sequential integers
# starting with 1. returns the length, or -1 if an error occurs.. leaves the
# indices of the source array "s" unchanged. the optional string "how" controls
# the direction and the comparison mode. uses the quick sort algorithm, with a
# random pivot to avoid worst-case behavior on already sorted arrays. requires
# the __compare() and __quicksort() functions.
# valid values for "how" are:
#   "std asc"
#     use awk's standard rules for comparison, ascending. this is the default
#   "std desc"
#     use awk's standard rules for comparison, descending.
#   "str asc"
#     force comparison as strings, ascending.
#   "str desc"
#     force comparison as strings, descending.
#   "num asc"
#     force a numeric comparison, ascending.
#   "num desc"
#     force a numeric comparison, descending.
BEGIN {
  # populate array
  for (i=10; i>0; i--) {
    a[i] = i;
  }

  # sort, numerically ascending
  len = qsort(a, b, "num asc");

  # dump
  for (i=1; i<=len; i++) {
    print b[i];
  }

  print "";
}

# usage: iqsort(s [, how])
# the bevavior is the same as that of qsort(), except that the array "s" is
# sorted in-place. the original indices are destroyed and replaced with
# sequential integers. everything else is described in qsort() above.
BEGIN {
  # populate array
  for (i=10; i>0; i--) {
    a[i] = i;
  }

  # sort in place, numerically ascending
  len = iqsort(a, "num asc");

  # dump
  for (i=1; i<=len; i++) {
    print a[i];
  }

  print "";
}

# usage: qsorti(s, d [, how])
# the behavior is the same as that of qsort(), except that the array indices
# are used for sorting, not the array values. when done, the new array is
# indexed numerically, and the values are those of the original indices.
# everything else is described in qsort() above.
BEGIN { 
  # populate array
  for (i=10; i>0; i--) {
    a[i];
  }

  # sort indices, numerically ascending
  len = qsorti(a, b, "num asc");

  # dump
  for (i=1; i<=len; i++) {
    print b[i];
  }

  print "";
}

# usage: iqsorti(s [, how])
# the bevavior is the same as that of qsorti(), except that the array "s" is
# sorted in-place. the original indices are destroyed and replaced with
# sequential integers. everything else is described in qsort() and qsorti()
# above.
BEGIN {
  # populate array
  for (i=10; i>0; i--) {
    a[i];
  }

  # sort indices in place, numerically ascending
  len = iqsorti(a, "num asc");

  # dump
  for (i=1; i<=len; i++) {
    print a[i];
  }

  print "";
}

# usage: qsortv(s, d [, how])
# sorts the indices in the array "s" based on the values, creating a new
# sorted array "d" indexed with sequential integers starting with 1, and the
# values the indices of "s". returns the length, or -1 if an error occurs.
# leaves the source array "s" unchanged. the optional string "how" controls
# the direction and the comparison mode. uses the quicksort algorithm, with a
# random pivot to avoid worst-case behavior on already sorted arrays. requires
# the __compare() and __vquicksort() functions. valid values for "how" are
# explained in the qsort() function above.
BEGIN {
  # populate array
  j=10
  for (i=1; i<=10; i++) {
    a[i] = j--;
  }

  # sort indices based on numeric values
  len = qsortv(a, b, "num asc");

  # dump
  for (i=1; i<=len; i++) {
    print b[i], a[b[i]];
  }

  print "";
}



# usage: shuf(s, d)
# shuffles the array "s", creating a new shuffled array "d" indexed with
# sequential integers starting with one. returns the length, or -1 if an error
# occurs. leaves the indices of the source array "s" unchanged. uses the knuth-
# fisher-yates algorithm. requires the __shuffle() function.
BEGIN {
  # populate array
  for (i=1; i<=10; i--) {
    a[i] = i;
  }

  # shuffle
  len = shuf(a, b);

  # dump
  for (i=1; i<=len; i++) {
    print b[i];
  }

  print "";
}

# usage: ishuf(s)
# the behavior is the same as that of shuf(), except the array "s" is sorted
# in-place. the original indices are destroyed and replaced with sequential
# integers. everything else is described in shuf() above.
BEGIN {
  # populate array
  for (i=1; i<=10; i--) {
    a[i] = i;
  }

  # shuffle in place
  len = ishuf(a);

  # dump
  for (i=1; i<=len; i++) {
    print a[i];
  }

  print "";
}

# usage: shufi(s, d)
# the bevavior is the same as that of shuf(), except that the array indices
# are shuffled, not the array values. when done, the new array is indexed
# numerically, and the values are those of the original indices. everything
# else is described in shuf() above.
BEGIN {
  # populate array
  for (i=1; i<=10; i--) {
    a[i] = i;
  }

  # shuffle indices
  len = shufi(a, b);

  # dump
  for (i=1; i<=len; i++) {
    print b[i];
  }

  print "";
}

# usage: ishufi(s)
# the behavior is tha same as that of shufi(), except that the array "s" is
# sorted in-place. the original indices are destroyed and replaced with
# sequential integers. everything else is describmed in shuf() and shufi()
# above.
BEGIN {
  # populate array
  for (i=1; i<=10; i--) {
    a[i] = i;
  }

  # shuffle indices in place
  len = ishufi(a);

  # dump
  for (i=1; i<=len; i++) {
    print a[i];
  }

  print "";
}
