// A function declaration.
int timesTwo(int x) {
  return x * 2;
}

// Arrow syntax is shorthand for `{ return expr; }`.
int timesFour(int x) => timesTwo(timesTwo(x));

// Functions are objects.
int runTwice(int x, int Function(int) f) {
  for (var i = 0; i < 2; i++) {
    x = f(x);
  }
  return x;
}

void main() {
  print('4 times two is ${timesTwo(4)}');
  print('4 times four is ${timesFour(4)}');
  print('2 x 2 x 2 is ${runTwice(2, timesTwo)}');
}