locals {
  a = true
  b = a
  c = 7 * 5
  d = c / 6
  e = c % 7
  f = 5 + c
  g = 7 - f
  h = 7 > 9
  i = 7 >= 9
  j = c < 3
  k = 34 <= 9
  l = 4 == 4
  m = 3 != c
  n = a && true
  o = true || false || b
  p = -6
  q = (7 + 5) * c
  r = a ? 1 : 5
}
