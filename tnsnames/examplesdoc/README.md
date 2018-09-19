# Tnsnames Grammar

## Tnanames Parser

The file `tnsnames_checker.java`, which can be found in the `examples` directory,
can be compiled to produce a parser that will read a `tnsnames.ora` file
on standard input and write any syntax error messages to standard error 
to allow you to correct the `tnsnames.ora` file.

A (valid) test file, `tnsnames.test.ora`, has been provided.
Play with this as you desire to see how well the parser works.
You might like to edit this file to create some errors to test the parser.

## Build the Parser

To build the parser, you need to have downloaded a copy of the ANTLR 4.x Jar file
from the [ANTLR4 web site](http://www.antlr.org/download.html).
Pick the Java target's "Complete ANTLR 4.x.x Java binaries jar" option.
This code was first developed using ANTLR 4.4 but later versions will/should work.

Follow the instructions on the [Getting Started page](https://github.com/antlr/antlr4/blob/master/doc/getting-started.md) paying particular attention to setting up `CLASSPATH` and the `grun` and `antlr4` aliases. Basically it is as follows, but the getting started page is a good read anyway!

```
export ANTLR4="/where/downloaded/to/antlr-4.x.x-complete.jar"
export CLASSPATH=".:${CLASSPATH}:${ANTLR4}"
alias antlr4='java org.antlr.v4.Tool'
alias grun='java org.antlr.v4.runtime.misc.TestRig'
alias jar='${JAVA_HOME}/bin/jar'
alias java='${JAVA_HOME}/bin/java'
alias javac='${JAVA_HOME}/bin/javac'
```

ANTLR 4 needs Java version 6, aka 1.6, or higher to compile and run.
I have tested my parser using Java 6 and Java 7 with no trouble.

* Copy the `*.g4` grammar files and the `tnsnames_checker.java` files into a working directory somewhere.
* In the working directory, compile the grammar with the command `antlr4 tnsnames.g4`.
This will generate a _lot_ of Java files.
* Compile the generated source code with `javac *.java`. This generates a _lot_ of class files.
* To avoid confusion and litter, build a jar. `jar -cvf tnsnames.jar *.class`
* You can delete the class files now ~~if you wish~~. `rm *.class`

You should now have a file named `tnsnames.jar`. 

## Run the Parser

Run it as follows:

```
java -cp "./tnsnames.jar:$CLASSPATH" tnsnames_checker < tnsnames.test.ora 
```

This will parse the `tnsnames.test.ora` file and report any errors it finds.
If you wish to redirect the errors to a file, try the following:

```
java -cp "./tnsnames.jar:$CLASSPATH" tnsnames_checker < tnsnames.test.ora 2> error.log
```

The file `error.log` now contains the list of errors for the test file.

## Enhancements

The parser only validates that the structure of your `tnsnames.ora` file fits the defined grammar.
It cannot check that port numbers are above 1024 and below 65536 for example,
or that a particular host name exists and can be pinged etc etc.

If you want to build this sort of validating tool then you appear to have two options:

1. Be my guest, go for it! All you will need to do is follow the instructions for ANTLR 4
on creating a listener and a parse tree walker and sub class the proper methods
of the generated base listener, and you can do whatever you want to do with a `tnsnames.ora` file.
2. Alternatively, I already did it for you!
[Full blown tnsnames parser](http://qdosmsq.dunbar-it.co.uk/blog/2014/12/tnsnames-checker-utility/)

Have fun.

## Contacts

Norman@dunbar-it.co.uk.
