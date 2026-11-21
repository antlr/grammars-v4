#!/usr/bin/awk -f

# comparison function for the *psort* functions
# usage: __pcompare(a, b, patterns, max, how)
# compares "a" and "b" based on "patterns" and "how", returning 0 for false and
# 1 for true. "patterns" is an indexed array of regexes, from 1 through "max".
# each regex takes priority over subsequent regexes, followed by non-matching
# values. required for all of the psort() functions below
function __pcompare(a, b, pattens, plen, how,    p) {
  # loop over each regex in order, and check if either value matches
  for (p=1; p<=plen; p++) {
    # if the first matches...
    if (a ~ p) {
      # check if the second also matches. if so, do a normal comparison
      if (b ~ p) {
        # standard comparisons
        if (how == "std asc") {
          return a < b;
        } else if (how == "std desc") {
          return a > b;

        # force string comps
        } else if (how == "str asc") {
          return "a" a < "a" b;
        } else if (how == "str desc") {
          return "a" a > "a" b;

        # force numeric
        } else if (how == "num asc") {
          return +a < +b;
        } else if (how == "num desc") {
          return +a > +b;
        }

      # if the second doesn't match, the first sorts higher
      } else {
        return 1;
      }

    # if the second matches but the first didn't, the second sorts higher
    } else if (b ~ p) {
      return 0;
    }
  }

  # no patterns matched, do a normal comparison
  return __compare(a, b, how);
}


# actual sorting function for the *psort* functions
# sorts the values in "array" in-place, from indices "left" to "right", based
# on "how" and the array "patterns" (see the psort() description)
# required for all of the psort() functions below
function __pquicksort(array, left, right, patterns, plen, how,
                      piv, mid, tmp) {
  # return if array contains one element or less
  if ((right - left) <= 0) {
    return;
  }

  # choose random pivot
  piv = int(rand() * (right - left + 1)) + left;

  # swap left and pivot
  tmp = array[piv];
  array[piv] = array[left];
  array[left] = tmp;
  
  mid = left;
  # iterate over each element from the second to the last, and compare
  for (piv=left+1; piv<=right; piv++) {
    # if the comparison based on "how" is true...
    if (__pcompare(array[piv], array[left], patterns, plen, how)) {
      # increment mid
      mid++;

      # swap mid and pivot
      tmp = array[piv];
      array[piv] = array[mid];
      array[mid] = tmp;
    }
  }

  # swap left and mid
  tmp = array[mid];
  array[mid] = array[left];
  array[left] = tmp;
  
  # recursively sort the two halves
  __pquicksort(array, left, mid - 1, patterns, plen, how);
  __pquicksort(array, mid + 1, right, patterns, plen, how);
}


## usage: psort(s, d, patts, max [, how])
## sorts the values of the array "s", based on the rules below. creates a new
## sorted array "d" indexed with sequential integers starting with 1. "patts"
## is a compact (*non-sparse) 1-indexed array containing regular expressions.
## "max" is the length of the "patts" array. returns the length of the "d"
## array. valid values for "how" are explained below. uses the quicksort
## algorithm, with a random pivot to avoid worst-case behavior on already sorted
## arrays. requires the __pcompare() and __pquicksort() functions.
##
##  Sorting rules:
##  - When sorting, values matching an expression in the "patts" array will
##    take priority over any other values
##  - Each expression in the "patts" array will have priority in ascending
##    order by index. "patts[1]" will have priority over "patts[2]" and
##    "patts[3]", etc
##  - Values both matching the same regex will be compared as usual
##  - All non-matching values will be compared as usual
##
## valid values for "how" are:
##   "std asc"
##     use awk's standard rules for comparison, ascending. this is the default
##   "std desc"
##     use awk's standard rules for comparison, descending.
##   "str asc"
##     force comparison as strings, ascending.
##   "str desc"
##     force comparison as strings, descending.
##   "num asc"
##     force a numeric comparison, ascending.
##   "num desc"
##     force a numeric comparison, descending.
function psort(array, out, patterns, plen, how,    count, i) {
  # make sure how is correct
  if (length(how)) {
    if (how !~ /^(st[rd]|num) (a|de)sc$/) {
      return -1;
    }

  # how was not passed, use the default
  } else {
    how = "std asc";
  }
  
  # loop over each index, and generate a new array with the same values and
  # sequential indices
  count = 0;
  for (i in array) {
    out[++count] = array[i];
  }

  # seed the random number generator
  srand();

  # actually sort
  __pquicksort(out, 1, count, patterns, plen, how);

  # return the length
  return count;
}

## usage: ipsort(s, patts, max [, how])
## the bevavior is the same as that of psort(), except that the array "s" is
## sorted in-place. the original indices are destroyed and replaced with
## sequential integers. everything else is described in psort() above.
function ipsort(array, patterns, plen, how,    tmp, count, i) {
  # make sure how is correct
  if (length(how)) {
    if (how !~ /^(st[rd]|num) (a|de)sc$/) {
      return -1;
    }

  # how was not passed, use the default
  } else {
    how = "std asc";
  }
  
  # loop over each index, and generate a new array with the same values and
  # sequential indices
  count = 0;
  for (i in array) {
    tmp[++count] = array[i];
    delete array[i];
  }

  # copy tmp back over array
  for (i=1; i<=count; i++) {
    array[i] = tmp[i];
    delete tmp[i];
  }

  # seed the random number generator
  srand();

  # actually sort
  __pquicksort(array, 1, count, patterns, plen, how);

  # return the length
  return count;
}

## usage: psorti(s, d, patts, max [, how])
## the behavior is the same as that of psort(), except that the array indices
## are used for sorting, not the array values. when done, the new array is
## indexed numerically, and the values are those of the original indices.
## everything else is described in psort() above.
function psorti(array, out, patterns, plen, how,    count, i) {
  # make sure how is correct
  if (length(how)) {
    if (how !~ /^(st[rd]|num) (a|de)sc$/) {
      return -1;
    }

  # how was not passed, use the default
  } else {
    how = "std asc";
  }

  # loop over each index, and generate a new array with the original indices
  # mapped to new numeric ones
  count = 0;
  for (i in array) {
    out[++count] = i;
  }

  # seed the random number generator
  srand();

  # actually sort
  __pquicksort(out, 1, count, patterns, plen, how);

  # return the length
  return count;
}

## usage: ipsorti(s, patts, max [, how])
## the bevavior is the same as that of psorti(), except that the array "s" is
## sorted in-place. the original indices are destroyed and replaced with
## sequential integers. everything else is described in psort() and psorti()
## above.
function ipsorti(array, patterns, plen, how,    tmp, count, i) {
  # make sure how is correct
  if (length(how)) {
    if (how !~ /^(st[rd]|num) (a|de)sc$/) {
      return -1;
    }

  # how was not passed, use the default
  } else {
    how = "std asc";
  }

  # loop over each index, and generate a new array with the original indices
  # mapped to new numeric ones
  count = 0;
  for (i in array) {
    tmp[++count] = i;
    delete array[i];
  }

  # copy tmp back over the original array
  for (i=1; i<=count; i++) {
    array[i] = tmp[i];
    delete tmp[i];
  }

  # seed the random number generator
  srand();

  # actually sort
  __pquicksort(array, 1, count, patterns, plen, how);

  # return the length
  return count;
}


# You can do whatever you want with this stuff, but a thanks is always
# appreciated
