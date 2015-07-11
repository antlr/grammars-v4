## Tnsnames Grammar - Examples

### Tnanames Parser

The file `tnsnames_checker.java` can be compiled to produce a parser that will read a `tnsnames.ora` file on standard input and write any syntax error messages to standard error to allow you to correct the `tnsnames.ora` file.

A (valid) test file, `tnsnames.test.ora`, has been provided. Play with this as you desire to see how well the parser works.

### Build the Parser

To build the parser, you need to have downloaded a copy of the [ANTLR 4-4 Jar file](http://www.antlr.org/download/antlr-4.4-complete.jar) from the [ANTLR4 web site](http://www.antlr.org/download.html). Pick the Java target's "complete" option.

Follow the instructions on the [Getting Started page](https://theantlrguy.atlassian.net/wiki/display/ANTLR4/Getting+Started+with+ANTLR+v4) paying particular attention to setting up `CLASSPATH` and the `grun` and `antlr4` aliases.

ANTLR 4 needs Java version 6, aka 1.6, to compile and run. I have tested my parser using Java 6 and Java 7 with no trouble.

* Copy the `tnsnames.g4` grammar and the `tnsnames_checker.java` files into a working directory somewhere.
* In the working directory, compile the grammar with the command `antlr4 tnsnames.g4`. This will generate a pile of Java files.
* Compile the generated source code with `javac *.java`. This generates a lot of class files.
* To avoid litter, build a jar. `jar -cvf tnsnames.jar *.class`
* You can delete the class files now, if you wish. `rm *.class`

You should now have a file named `tnsnames.jar`. 

### Run the Parser

Run it as follows:

```
java -cp "./tnsnames.jar:$CLASSPATH" tnsnames_checker < tnsnames.test.ora 
```

This will parse the `tnsnames.test.ora` file and report any errors it finds. If you wish to redirect the errors to a file, try the following:

```
java -cp "./tnsnames.jar:$CLASSPATH" tnsnames_checker < tnsnames.test.ora 2> error.log
```

The file `error.log` now contains the list of errors for the test file.

### Enhancements

The parser only validates that the structure of your `tnsnames.ora` file fits the defined grammar. It cannot check that port numbers are above 1024 and below 65536 for example, or that a particular host name exists and can be pinged etc etc.

If you want to build this sort of validating tool, be my guest! All you will need to do is follow the instructions for ANTLR 4 on creating a listener and a parse tree walker and sub class the proper methods of the generated base listener, and you can do whetver you want to do with a `tnsnames.ora` file.


Have fun.






Norman@dunbar-it.co.uk.
