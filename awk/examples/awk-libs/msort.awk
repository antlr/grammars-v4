#!/usr/bin/awk -f

# comparison function
# usage: __compare(a, b, how)
# compares "a" and "b" based on "how", returning 0 for false and 1 for true.
# required for all of the msort() functions below
function __compare(a, b, how) {
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
}

# actual sorting function
# usage: __mergesort(array, len, how)
# sorts the values in "array" in-place, from indices 1 to "len", based
# on the comparison mode "how" (see the msort() description).
# required for all of the msort() functions below
function __mergesort(array, len, how,
                     tmpa, alen, a, tmpb, blen, b, half, cur, pos, tmp) {
  # if there are 10 elements or less, use an insertion sort and return
  if (len <= 10) {
    # loop over each item, starting with the second
    for (cur=2; cur<=len; cur++) {
      pos = cur;
      # shift the item down the list into position
      while (pos > 1 && __compare(array[pos], array[pos-1], how)) {
        tmp = array[pos];
        array[pos] = array[pos-1];
        array[pos-1] = tmp;

        pos--;
      }
    }

    # return
    return len;
  }

  # determine the halfway point of the indices
  half = int(len / 2);

  # create temp arrays of the two halves
  a = 0;
  for (i=1; i<=half; i++) {
    tmpa[++a] = array[i];

    # remove the index from the original array
    delete array[i];
  }
  b = 0;
  for (i=half+1; i<=len; i++) {
    tmpb[++b] = array[i];

    # remove the index from the original array
    delete array[i];
  }

  # sort the two halves with recursive calls
  alen = __mergesort(tmpa, a, how);
  blen = __mergesort(tmpb, b, how);

  # merge the two halves
  len = 0;
  a = b = 1;
  # loop while there is still an element in either array
  while (a <= alen || b <= blen) {
    # a sorts first
    if (a <= alen && (b > blen || __compare(tmpa[a], tmpb[b], how))) {
      array[++len] = tmpa[a];
      delete tmpa[a++]; # remove the index from the temp array

    # b sorts first
    } else {
      array[++len] = tmpb[b];
      delete tmpb[b++]; # remove the index from the temp array
    }
  }

  # return the length
  return len;
}

# actual sorting function for the msortv() function
# usage: __mergesortv(array, values, len, how)
# sorts the values in "array" on the original values in "values", from indices
# 1 through "len", based on the comparison mode "how" (see the msortv()
# description). required for all of the msortv() functions below
function __mergesortv(array, values, len, how,
                      tmpa, tmpva, alen, a, tmpb, tmpvb, blen, b,
                      half, cur, pos, tmp) {
  # if there are 10 elements or less, use an insertion sort and return
  if (len <= 10) {
    # loop over each item, starting with the second
    for (cur=2; cur<=len; cur++) {
      pos = cur;
      # shift the item down the list into position
      while (pos > 1 && __compare(values[pos], values[pos-1], how)) {
        tmp = array[pos];
        array[pos] = array[pos-1];
        array[pos-1] = tmp;
        tmp = values[pos];
        values[pos] = values[pos-1];
        values[pos-1] = tmp;

        pos--;
      }
    }

    # return
    return len;
  }

  # determine the halfway point of the indices
  half = int(len / 2);

  # create temp arrays of the two halves
  a = 0;
  for (i=1; i<=half; i++) {
    tmpa[++a] = array[i];
    tmpva[a] = values[i];

    # remove the index from the original array
    delete array[i];
  }
  b = 0;
  for (i=half+1; i<=len; i++) {
    tmpb[++b] = array[i];
    tmpvb[b] = values[i];

    # remove the index from the original array
    delete array[i];
  }

  # sort the two halves with recursive calls
  alen = __mergesortv(tmpa, tmpva, a, how);
  blen = __mergesortv(tmpb, tmpvb, b, how);

  # merge the two halves
  len = 0;
  a = b = 1;
  # loop while there is still an element in either array
  while (a <= alen || b <= blen) {
    # a sorts first
    if (a <= alen && (b > blen || __compare(tmpva[a], tmpvb[b], how))) {
      array[++len] = tmpa[a];
      values[len] = tmpva[a];
      delete tmpva[a];
      delete tmpa[a++]; # remove the index from the temp array

    # b sorts first
    } else {
      array[++len] = tmpb[b];
      values[len] = tmpvb[b];
      delete tmpvb[b];
      delete tmpb[b++]; # remove the index from the temp array
    }
  }

  # return the length
  return len;
}



## usage: msort(s, d [, how])
## sorts the elements in the array "s" using awk's normal rules for comparing
## values, creating a new sorted array "d" indexed with sequential integers
## starting with 1. returns the length, or -1 if an error occurs.. leaves the
## indices of the source array "s" unchanged. the optional string "how" controls
## the direction and the comparison mode. uses the merge sort algorithm, with an
## insertion sort when the list size gets small enough. this is not a stable
## sort. requires the __compare() and __mergesort() functions.
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
function msort(array, out, how,    count, i) {
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

  # actually sort
  return __mergesort(out, count, how);
}

## usage: imsort(s [, how])
## the bevavior is the same as that of msort(), except that the array "s" is
## sorted in-place. the original indices are destroyed and replaced with
## sequential integers. everything else is described in msort() above.
function imsort(array, how,    tmp, count, i) {
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

  # actually sort
  return __mergesort(array, count, how);
}

## usage: msorti(s, d [, how])
## the behavior is the same as that of msort(), except that the array indices
## are used for sorting, not the array values. when done, the new array is
## indexed numerically, and the values are those of the original indices.
## everything else is described in msort() above.
function msorti(array, out, how,    count, i) {
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

  # actually sort
  return __mergesort(out, count, how);
}

## usage: imsorti(s [, how])
## the bevavior is the same as that of msorti(), except that the array "s" is
## sorted in-place. the original indices are destroyed and replaced with
## sequential integers. everything else is described in msort() and msorti()
## above.
function imsorti(array, how,    tmp, count, i) {
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

  # actually sort
  return __mergesort(array, count, how);
}

## usage: msortv(s, d [, how])
## sorts the indices in the array "s" based on the values, creating a new
## sorted array "d" indexed with sequential integers starting with 1, and the
## values the indices of "s". returns the length, or -1 if an error occurs.
## leaves the source array "s" unchanged. the optional string "how" controls
## the direction and the comparison mode. uses the merge sort algorithm, with
## an insertion sort when the list size gets small enough. this is not a stable
## sort. requires the __compare() and __mergesortv() functions. valid values for
## "how" are explained in the msort() function above.
function msortv(array, out, how,    values, count, i) {
  # make sure how is correct
  if (length(how)) {
    if (how !~ /^(st[rd]|num) (a|de)sc$/) {
      return -1;
    }

  # how was not passed, use the default
  } else {
    how = "std asc";
  }

  # loop over each index, and generate two new arrays: the original indices
  # mapped to numeric ones, and the values mapped to the same indices
  count = 0;
  for (i in array) {
    count++;
    out[count] = i;
    values[count] = array[i];
  }

  # actually sort
  return __mergesortv(out, values, count, how);
}



# You can do whatever you want with this stuff, but a thanks is always
# appreciated
