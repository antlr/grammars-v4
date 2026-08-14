#!/usr/bin/awk -f

# include library. gawk can use AWKPATH so the actual path isn't needed, see
# the man page (since the path is relative, this assumes the lib dir is in
# AWKPATH or the same dir)
@include "math.awk";

# usage: abs(number)
# returns the absolute value of "number"
BEGIN {
  print abs(-2.3);
  print abs(0);
  print abs(2.3);
}

# usage: ceil(number)
# returns "number" rounded UP to the nearest int
BEGIN {
  print ceil(2.3);
  print ceil(-2.3);
}

# usage: ceiling(multiple, number)
# returns "number" rounded UP to the nearest multiple of "multiple"
BEGIN {
  # nearest multiple of 5 above 6 is 10
  print ceiling(5, 6);
}

# usage: floor(multiple, number)
# returns "number" rounded DOWN to the nearest multiple of "multiple"
BEGIN {
  # nearest multiple of 5 below 9 is 5
  print floor(5, 9);
}

# usage: round(multiple, number)
# returns "number" rounded to the nearest multiple of "multiple"
BEGIN {
  # nearest multiple of 5 to 8 is 10
  print round(5, 8);
}

# usage: rint(number)
# returns "number" rounded to the nearest integer
BEGIN {
  # round to the nearest int
  print rint(1.3), rint(1.5), rint(2.8)
}

# usage: change_base(number, start_base, end_base)
# converts "number" from "start_base" to "end_base"
# bases must be between 2 and 64. the digits greater than 9 are represented
# by the lowercase letters, the uppercase letters, @, and _, in that order.
# if ibase is less than or equal to 36, lowercase and uppercase letters may
# be used interchangeably to represent numbers between 10 and 35.
# returns 0 if any argument is invalid
BEGIN {
  # convert '3' to binary:
  print change_base(3, 10, 2);

  # convert '111' in binary to decimal:
  print change_base(111, 2, 10);

  # convert 111 in binary to hex:
  print change_base(111, 2, 16);

  # convert 1f in hex to decimal:
  print change_base("1f", 16, 10);
}

# usage: format_num(number)
# adds commas to "number" to make it more readable. for example,
# format_num(1000) will return "1,000", and format_num(123456.7891) will
# return "123,456.7891". also trims leading zeroes
# returns 0 if "number" is not a valid number
BEGIN {
  num = "1000";
  printf("unformatted: %s\nformatted: %s\n\n", num, format_num(num));

  num = "123456.7891";
  printf("unformatted: %s\nformatted: %s\n\n", num, format_num(num));
}

# usage: str_to_num(string)
# examines "string", and returns its numeric value. if "string" begins with a
# leading 0, assumes that "string" is an octal number. if "string" begins with
# a leading "0x" or "0X", assumes that "string" is a hexadecimal number.
# otherwise, decimal is assumed.
BEGIN {
  num = "12";
  printf("str_to_num(\"%s\") == %s\n", num, str_to_num(num));
  num = "012";
  printf("str_to_num(\"%s\") == %s\n", num, str_to_num(num));
  num = "0x12";
  printf("str_to_num(\"%s\") == %s\n", num, str_to_num(num));

  print "";
}

# usage: isint(string)
# returns 1 if "string" is a valid integer, otherwise 0
BEGIN {
  var = "3";
  if (isint(var)) {
    print var " is a valid integer";
  } else {
    print var " is not a valid integer";
  }


  var = "1.34";
  if (isint(var)) {
    print var " is a valid integer";
  } else {
    print var " is not a valid integer";
  }

  var = "foo";
  if (isint(var)) {
    print var " is a valid integer";
  } else {
    print var " is not a valid integer";
  }
}

# usage: isnum(string)
# returns 1 if "string" is a valid number, otherwise 0
BEGIN {
  var = "3";
  if (isnum(var)) {
    print var " is a valid number";
  } else {
    print var " is not a valid number";
  }


  var = "1.34";
  if (isnum(var)) {
    print var " is a valid number";
  } else {
    print var " is not a valid number";
  }

  var = "foo";
  if (isnum(var)) {
    print var " is a valid number";
  } else {
    print var " is not a valid number";
  }
}

# usage: isprime(number)
# returns 1 if "number" is a prime number, otherwise 0. "number" must be a
# positive integer
BEGIN {
  print "primes from 1 through 10:";
  for (i=1; i<=10; i++) {
    if (isprime(i)) {
      printf("%s ", $i);
    }
  }
  print "";
}

# usage: gcd(a, b)
# returns the greatest common denominator (greatest common factor) of a and b.
# both a and b must be positive integers. uses the recursive euclid algorithm.
BEGIN {
  print "the greatest common factor of 3 and 6 is", gcd(3, 6);
}

# usage: lcm(a, b)
# returns the least common multiple of a and b. both a and b must be positive
# integers.
BEGIN {
  print "the least common multiple of 4 and 16 is", lcm(4, 16);
}

# usage: calc_e()
# approximates e by calculating the sumation from k=0 to k=50 of 1/k!
# returns 10 decimal places
BEGIN {
  # prints e
  print "e is approximately " calc_e();
}

# usage: calc_pi()
# returns pi, with an accuracy of 10 decimal places
BEGIN {
  # prints pi
  print "pi is approximately " calc_pi();
}

# usage: calc_tau()
# returns tau, with an accuracy of 10 decimal places
# http://tauday.com/tau-manifesto
BEGIN {
  # prints tau
  print "pi is wrong! tau is approximately " calc_tau();
}

# usage: deg_to_rad(degrees)
# converts degrees to radians
BEGIN {
  # convert 90 degrees to radians
  print "90 degrees is " deg_to_rad(90) " radians";
}

# usage: rad_to_deg(radians)
# converts radians to degrees
BEGIN {
  # convert pi radians to degrees
  print "pi radians is " rad_to_deg(calc_pi()) " degrees";
}

# usage: tan(expr)
# returns the tangent of expr, which is in radians
BEGIN {
  # print the tangent of pi radians
  print "the tan of pi radians is " tan(calc_pi());
}

# usage: csc(expr)
# returns the cosecant of expr, which is in radians
BEGIN {
  # print the cosecant of pi radians
  print "the csc of pi radians is " csc(calc_pi());
}

# usage: sec(expr)
# returns the secant of expr, which is in radians
BEGIN {
  # print the secant of pi radians
  print "the sec of pi radians is " sec(calc_pi());
}

# usage: cot(expr)
# returns the cotangent of expr, which is in radians
BEGIN {
 # print the cotangent of pi radians
  print "the cot of pi radians is " cot(calc_pi());
}
