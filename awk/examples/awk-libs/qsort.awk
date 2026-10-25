#!/usr/bin/awk -f

# comparison function
# usage: __compare(a, b, how)
# compares "a" and "b" based on "how", returning 0 for false and 1 for true.
# required for all of the qsort() functions below
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
# sorts the values in "array" in-place, from indices "left" to "right", based
# on the comparison mode "how" (see the qsort() description).
# required for all of the qsort() functions below
function __quicksort(array, left, right, how,    piv, mid, tmp) {
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
    if (__compare(array[piv], array[left], how)) {
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
  __quicksort(array, left, mid - 1, how);
  __quicksort(array, mid + 1, right, how);
}

# actual sorting function for the qsortv() function
# sorts the indices in "array" on the original values in "values", from indices
# "left" to "right", based on the comparison mode "how" (see the qsortv()
# description)
# required for the qsortv() function below
function __vquicksort(array, values, left, right, how,    piv, mid, tmp) {
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
  tmp = values[piv];
  values[piv] = values[left];
  values[left] = tmp;
  
  mid = left;
  # iterate over each element from the second to the last, and compare
  for (piv=left+1; piv<=right; piv++) {
    # if the comparison based on "how" is true...
    if (__compare(values[piv], values[left], how)) {
      # increment mid
      mid++;

      # swap mid and pivot
      tmp = array[piv];
      array[piv] = array[mid];
      array[mid] = tmp;
      tmp = values[piv];
      values[piv] = values[mid];
      values[mid] = tmp;
    }
  }

  # swap left and mid
  tmp = array[mid];
  array[mid] = array[left];
  array[left] = tmp;
  tmp = values[mid];
  values[mid] = values[left];
  values[left] = tmp;
  
  # recursively sort the two halves
  __vquicksort(array, values, left, mid - 1, how);
  __vquicksort(array, values, mid + 1, right, how);
}



## usage: qsort(s, d [, how])
## sorts the elements in the array "s" using awk's normal rules for comparing
## values, creating a new sorted array "d" indexed with sequential integers
## starting with 1. returns the length, or -1 if an error occurs.. leaves the
## indices of the source array "s" unchanged. the optional string "how" controls
## the direction and the comparison mode. uses the quick sort algorithm, with a
## random pivot to avoid worst-case behavior on already sorted arrays. this is
## not a stable sort. requires the __compare() and __quicksort() functions.
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
function qsort(array, out, how,    count, i) {
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
  __quicksort(out, 1, count, how);

  # return the length
  return count;
}

## usage: iqsort(s [, how])
## the bevavior is the same as that of qsort(), except that the array "s" is
## sorted in-place. the original indices are destroyed and replaced with
## sequential integers. everything else is described in qsort() above.
function iqsort(array, how,    tmp, count, i) {
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
  __quicksort(array, 1, count, how);

  # return the length
  return count;
}

## usage: qsorti(s, d [, how])
## the behavior is the same as that of qsort(), except that the array indices
## are used for sorting, not the array values. when done, the new array is
## indexed numerically, and the values are those of the original indices.
## everything else is described in qsort() above.
function qsorti(array, out, how,    count, i) {
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
  __quicksort(out, 1, count, how);

  # return the length
  return count;
}

## usage: iqsorti(s [, how])
## the bevavior is the same as that of qsorti(), except that the array "s" is
## sorted in-place. the original indices are destroyed and replaced with
## sequential integers. everything else is described in qsort() and qsorti()
## above.
function iqsorti(array, how,    tmp, count, i) {
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
  __quicksort(array, 1, count, how);

  # return the length
  return count;
}

## usage: qsortv(s, d [, how])
## sorts the indices in the array "s" based on the values, creating a new
## sorted array "d" indexed with sequential integers starting with 1, and the
## values the indices of "s". returns the length, or -1 if an error occurs.
## leaves the source array "s" unchanged. the optional string "how" controls
## the direction and the comparison mode. uses the quicksort algorithm, with a
## random pivot to avoid worst-case behavior on already sorted arrays. this is
## not a stable sort. requires the __compare() and __vquicksort() functions.
## valid values for "how" are explained in the qsort() function above.
function qsortv(array, out, how,    values, count, i) {
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

  # seed the random number generator
  srand();

  # actually sort
  __vquicksort(out, values, 1, count, how);

  # return the length
  return count;
}



# You can do whatever you want with this stuff, but a thanks is always
# appreciated
