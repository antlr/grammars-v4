#!/usr/bin/awk -f

## usage: abs(number)
## returns the absolute value of "number"
function abs(num) {
  return num < 0 ? -num : num;
}

## usage: ceil(number)
## returns "number" rounded UP to the nearest int
function ceil(num) {
  if (num < 0) {
    return int(num);
  } else {
    return int(num) + (num == int(num) ? 0 : 1);
  }
}

## usage: ceiling(multiple, number)
## returns "number" rounded UP to the nearest multiple of "multiple"
function ceiling(mult, num,    r) {
  return (r = num % mult) ? num + (mult - r) : num;
}

## usage: change_base(number, start_base, end_base)
## converts "number" from "start_base" to "end_base"
## bases must be between 2 and 64. the digits greater than 9 are represented
## by the lowercase letters, the uppercase letters, @, and _, in that order.
## if ibase is less than or equal to 36, lowercase and uppercase letters may
## be used interchangeably to represent numbers between 10 and 35.
## returns 0 if any argument is invalid
function change_base(num, ibase, obase,
                     chars, c, l, i, j, cur, b10, f, fin, isneg) {
  # convert number to lowercase if ibase <= 36
  if (ibase <= 36) {
    num = tolower(num);
  }

  # determine if number is negative. if so, set isneg=1 and remove the '-'
  if (sub(/^-/, "", num)) {
    isneg = 1;
  }

  # determine if inputs are valid
  if (num ~ /[^[:xdigit:]]/ || ibase != int(ibase) || obase != int(obase) ||
      ibase < 2 || ibase > 64 || obase < 2 || obase > 64) {
    return 0;
  }

  # set letters to numbers conversion array
  if (ibase > 10 || obase > 10) {
    # set chars[] array to convert letters to numbers
    c = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ@_";
    l = length(c);

    j = 10;
    for (i=1; i<=l; i++) {
      cur = substr(c, i, 1);
      chars[cur] = j;
      chars[j] = cur;

      j++;
    }
  }
  
  # convert to base 10
  if (ibase != 10) { 
    l = length(num);

    j = b10 = 0;
    for (i=l; i>0; i--) {
      c = substr(num, i, 1);

      # if char is a non-digit convert to dec
      if (c !~ /^[0-9]$/) {
        c = chars[c];
      }

      # check to make sure value isn't too great for base
      if (+c >= ibase) {
        return 0;
      }

      b10 += c * (ibase ^ j++);
    }
  } else {
    # num is already base 10
    b10 = num;
  }
  
  # convert from base 10 to obase
  if (obase != 10) {
    # build number backwards
    j = 0;
    do {
      f[++j] = (c = b10 % obase) > 9 ? chars[c] : c;
      b10 = int(b10 / obase);
    } while (b10);

    # reverse number
    fin = f[j];
    for (i=j-1; i>0; i--) {
      fin = fin f[i];
    }
  } else {
    # num has already been converted to base 10
    fin = b10;
  }

  # add '-' if number was negative
  if (isneg) {
    fin = "-" fin;
  }

  return fin;
}

## usage: format_num(number)
## adds commas to "number" to make it more readable. for example,
## format_num(1000) will return "1,000", and format_num(123456.7890) will
## return "123,456.7890". also trims leading zeroes
## returns 0 if "number" is not a valid number
function format_num(num,    is_float, b, e, i, len, r, out) {
  # trim leading zeroes
  sub(/^0+/, "", num);

  # make sure "num" is a valid number
  if (num ~ /[^0-9.]/ || num ~ /\..*\./) {
    return 0;
  }
  
  # if "num" is not an int, split it into pre and post decimal parts.
  # use sub() instead of int() because int() can be funny for float arithmetic
  # results
  if (num ~ /\./) {
    is_float = 1; # flag "num" as a float
    b = e = num;
    sub(/\..*/, "", b);
    sub(/.*\./, "", e);

  # otherwise, just assign the number to "b"
  } else {
    is_float = 0;
    b = num;
  }

  len = length(b)

  # only do anything if the pre-decimal section is greater than 3 digits
  if (len < 3) {
    return num;
  }

  # start by assigning the last 3 pre-decimal digits to out
  out = substr(b, len - 2);

  # loop backwards over each grouping of 3 numbers after that, prepending
  # each to out (with a comma)
  for (i=len-5; i>0; i-=3) {
    out = substr(b, i, 3) "," out;
  }

  # if the length is not a multiple of 3, prepend the remaining digits
  if (r = len % 3) {
    out = substr(b, 1, r) "," out;
  }

  # if number was a float, add the post-decimal digits back on
  if (is_float) {
    out = out "." e;
  }

  # return the formatted number
  return out;
}

## usage: str_to_num(string)
## examines "string", and returns its numeric value. if "string" begins with a
## leading 0, assumes that "string" is an octal number. if "string" begins with
## a leading "0x" or "0X", assumes that "string" is a hexadecimal number.
## otherwise, decimal is assumed.
function str_to_num(str,    base, isneg, l, i, j, chars, c, num) {
  # convert to all lowercase
  str = tolower(str);

  # determine if number is negative. if so, set isneg=1 and remove the '-'
  if (sub(/^-/, "", num)) {
    isneg = 1;
  }

  # examine the string, to determine the base and trim said base information
  if (sub(/^0x/, "", str)) {
    base = 16;
  } else if (sub(/^0/, "", str)) {
    base = 8;
  } else {
    base = 10;
  }

  # trim everything from the first non-number character to the end
  if (base == 16) {
    sub(/[^[:xdigit:]].*/, "", str);
  } else {
    sub(/[^[:digit:]].*/, "", str);
  }

  # if the base is octal, but there's a number >= 8, set it to decimal instead
  if (base == 8 && str ~ /[89]/) {
    base = 10;
  }

  # don't need to convert if the base is 10
  if (base == 10) {
    return isneg ? -str : +str;
  }

  # set letters for hex
  if (base == 16) {
    chars["a"] = 10; chars["b"] = 11; chars["c"] = 12;
    chars["d"] = 13; chars["e"] = 14; chars["f"] = 15;
  }

  # convert to base 10
  l = length(str);

  j = num = 0;
  for (i=l; i>0; i--) {
    c = substr(str, i, 1);

    # if char is a non-digit convert to dec
    if (c !~ /^[0-9]$/) {
      c = chars[c];
    }

    num += c * (base ^ j++);
  }
  
  # return the number
  return isneg ? -num : +num;
}

## usage: floor(multiple, number)
## returns "number" rounded DOWN to the nearest multiple of "multiple"
function floor(mult, num) {
  return num - (num % mult);
}

## usage: round(multiple, number)
## returns "number" rounded to the nearest multiple of "multiple"
function round(mult, num,    r) {
  if (num % mult < mult / 2) {
    return num - (num % mult);
  } else {
    return (r = num % mult) ? num + (mult - r) : num;
  }
}

## usage: rint(number)
## returns "number" rounded to the nearest integer
function rint(num,    n) {
  if (num < 0) {
    return (num - (n = int(num)) < -.5) ? n - 1 : n;
  } else {
    return (num - (n = int(num)) >= .5) ? n + 1 : n;
  }
}

## usage: isint(string)
## returns 1 if "string" is a valid integer, otherwise 0
function isint(str) {
  if (str !~ /^-?[0-9]+$/) {
    return 0;
  }

  return 1;
}

## usage: isnum(string)
## returns 1 if "string" is a valid number, otherwise 0
function isnum(str) {
  # use a regex comparison because 'str == str + 0' has issues with some floats
  if (str !~ /^-?[0-9.]+$/ || str ~ /\..*\./) {
    return 0;
  }

  return 1;
}

## usage: isprime(number)
## returns 1 if "number" is a prime number, otherwise 0. "number" must be a
## positive integer greater than one
function isprime(num,    i, s) {
  # check to make sure "num" is a valid positive int (and not 1)
  if (num !~ /^[0-9]+$/ || num <= 1) {
    return 0;
  }

  # 1, 2, and 3 are prime
  if (num <= 3) {
    return 1;
  }
  
  # check if even or divisible by 3
  if (!(num % 2) || !(num % 3)) {
    return 0;
  }
  
  # use naive method, fermats little theorem had overflow and did not work
  # for primes larger than 1021
  s = sqrt(num);
  for (i=5; i<=s; i+=2) {
    if (!(num % i)) {
      return 0;
    }
  }

  return 1;
}

## usage: gcd(a, b)
## returns the greatest common denominator (greatest common factor) of a and b.
## both a and b must be positive integers. uses the recursive euclid algorithm.
function gcd(a, b,    f) {
  # check to make sure both numbers are positive ints
  if (!f) {
    if (a !~ /^[0-9]+$/ || !a || b !~ /^[0-9]+$/ || !b) {
      return 0;
    }
  }

  if (b) {
    return gcd(b, a % b, 1);

  } else {
    # return the absolute value
    return a < 0 ? -a : a;
  }
}

## usage: lcm(a, b)
## returns the least common multiple of a and b. both a and b must be positive
## integers.
function lcm(a, b,    m, l) {
  # check to make sure both numbers are positive ints
  if (a !~ /^[0-9]+$/ || !a || b !~ /^[0-9]+$/ || !b) {
    return 0;
  }

  m = 0;
  while ((l = ++m * a) % b);

  return l;
}

## usage: calc_e()
## approximates e by calculating the sumation from k=0 to k=50 of 1/k!
## returns 10 decimal places
function calc_e(lim,    e, k, i, f) {
  for (k=0; k<=50; k++) {
    # calculate factorial
    f = 1;
    for (i=1; i<=k; i++) {
      f = f * i;
    }

    # add to e
    e += 1 / f;
  }

  return sprintf("%0.10f", e);
}


## usage: calc_pi()
## returns pi, with an accuracy of 10 decimal places
function calc_pi() {
  return sprintf("%0.10f", atan2(0, -1));
}

## usage: calc_tau()
## returns tau, with an accuracy of 10 decimal places
function calc_tau() {
  return sprintf("%0.10f", 2 * atan2(0, -1));
}

## usage: deg_to_rad(degrees)
## converts degrees to radians
function deg_to_rad(deg,    tau) {
  tau = 8 * atan2(1,1);

  return (deg/360) * tau;
}

## usage: rad_to_deg(radians)
## converts radians to degrees
function rad_to_deg(rad,    tau) {
  tau = 8 * atan2(1,1);

  return (rad/tau) * 360;
}

## usage: tan(expr)
## returns the tangent of expr, which is in radians
function tan(ang) {
  return sin(ang)/cos(ang);
}

## usage: csc(expr)
## returns the cosecant of expr, which is in radians
function csc(ang) {
  return 1/sin(ang);
}

## usage: sec(expr)
## returns the secant of expr, which is in radians
function sec(ang) {
  return 1/cos(ang);
}

## usage: cot(expr)
## returns the cotangent of expr, which is in radians
function cot(ang) {
  return cos(ang)/sin(ang);
}



# You can do whatever you want with this stuff, but a thanks is always
# appreciated
