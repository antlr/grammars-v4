import 'dart:math' as math;

void main() {
  print('a single quoted string');
  print("a double quoted string");

  // Strings can be combined by placing them adjacent to each other.
  print('cat' 'dog');

  // Triple quotes define a multi-line string.
  print('''triple quoted strings
are for multiple lines''');

  // Dart supports string interpolation.
  final pi = math.pi;
  print('pi is $pi');
  print('tau is ${2 * pi}');
}