The tests in this subdirectory do not pass structures as arguments or return values, although some do pass pointers to structures.
Tests are organized into the following directories:
* `smoke_tests/`: Basic tests of struct type declarations, compound initializers, and member access.
* `parse_and_lex/`: Tests to make sure the parser and lexer don't reject certain edge cases.
* `semantic_analysis/`: Tests focused on identifier resolution and type checking. The validate that we correctly resolve tags, track when types are complete, and can handle valid uses of incomplete structure types.
* `size_and_offset_calculations/`: Tests validating that we calculate structure sizes and member offsets correctly. These use the `&` and `sizeof` operators to determine sizes and offsets without actually reading or writing any structures or members.
* `scalar_member_access/`: Tests for the `.` and `->` operators. Some of these tests combine operators to access nested scalar members, but none require copying an entire structure to another location.
* `struct_copy/`: Tests that involve copying entire structures, including assigning one structure to another and reading/writing entire structures through `.`, `->`, and `[]` operators.
* `libraries/`: Test programs that include multiple translation units. Tests in `libraries/initializers/` validate that structure initializers initialize the correct values at the correct locations in a structure. Other tests validate that global structure variables can be accessed in all translation units, and that pointers to structures can be passed as parameters and return values.

Recommended debugging order:
1. `smoke_tests/`
2. `size_and_offset_calculations/`
3. `scalar_member_access/`
4. `libraries/initializers/`
5. `parse_and_lex/`
6. `struct_copy/`
7. `semantic_analysis/`
8. Remaining tests in `libraries/`
