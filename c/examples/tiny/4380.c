// This example illustrates ambiguity in the chose of a statement vs.
// declaration in blockItem. The parse cannot be a statement if it
// begins with a type.
// See https://groups.google.com/g/antlr-discussion/c/nGvUxmnOxsI
//
// NB!! This code cannot parse if you do not declare "nginx" as a type,
// which was incorrectly done here:
// https://github.com/antlr/grammars-v4/blob/d4e116f84f28ed73d27d39032b1751dda0151b63/c/examples/4380.c#L8
// Declaring a struct with tag does not declare the type. Struct tags
// are not the same as types. I have confirmed with GCC.

typedef struct nginx {} nginx;

int main() {
	nginx *d;
}
