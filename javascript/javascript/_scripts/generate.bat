antlr -o gen *.g4 && cd gen && copy /Y ..\Java\* . && javac *.java && cd ..
