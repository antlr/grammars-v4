Most of the tests in this directory exercise type checking of union types. A few test the interaction between support for structure types and earlier extra credit features.

These tests are organized into the following subdirectories:
* `bad_union_member_access/`: `.` and `->` operatiosn with bad member names
* `incompatible_union_types/`: uses of incompatible union types in assignments, function calls, conditional expressions, etc.
* `incomplete_unions/`: uses of incomplete union types where complete types are required
* `invalid_union_lvalues/`: uses of non-lvalues of union type where lvalues are required
* `other_features/`: all tests of extra credit features other than union types
* `scalar_required/`: uses of unions where scalar types (or more specific scalar types, e.g. integers) are required
* `union_initializers/`: invalid initializers for union types
* `union_struct_conflicts/`: using the same tag with both the `struct` and `union` specifiers, in contexts where both uses of the tag should specify the same type.
* `union_tag_resolution/`: errors caused by incorrect tag resolution (typically leading to incompatible types in assignment etc)
* `union_type_declarations/`: invalid definitions, e.g. duplicate member names, members w/ incomplete types
