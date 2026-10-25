# take from https://cmake.org/cmake/help/v3.12/manual/cmake-language.7.html
add_executable(hello world.c)

if(FALSE AND (FALSE OR (HELLO WORLD) TRUE)) # evaluates to FALSE

message([=[
This is the first line in a bracket argument with bracket length 1.
No \-escape sequences or ${variable} references are evaluated.
This is always one argument even though it contains a ; character.
The text does not end on a closing bracket of length 0 like ]].
It does end in a closing bracket of length 1.
]=])

message("This is a quoted argument containing multiple lines.
This is always one argument even though it contains a ; character.
Both \\-escape sequences and ${variable} references are evaluated.
The text does not end on an escaped double-quote like \".
It does end in an unescaped double quote.
")

message("\
This is the first line of a quoted argument. \
In fact it is the only line but since it is long \
the source code uses line continuation.\
")

foreach(arg
    NoSpace
    Escaped\ Space
    This;Divides;Into;Five;Arguments
    Escaped\;Semicolon
    )
  message("${arg}")
endforeach()

#[[This is a bracket comment.
It runs until the close bracket.]]
message("First Argument\n" #[[Bracket Comment]] "Second Argument")

# This is a line comment.
message("First Argument\n" # This is a line comment :)
        "Second Argument") # This is a line comment.

set(srcs a.c b.c c.c) # sets "srcs" to "a.c;b.c;c.c"

set(x a "b;c") # sets "x" to "a;b;c", not "a;b\;c"
