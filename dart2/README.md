# DartGrammar

Conversion of the combined Antlr4 grammar
https://github.com/dart-lang/sdk/blob/main/tools/spec_parser/Dart.g
to "target-agnostic format".

## Notes
* Targets supported: CSharp, Dart, Java.
* Use `trgen -t <target>; cd Generated; make` to build the grammar with driver.
* All targets use the same grammar.

Ken Domino, April 2022
