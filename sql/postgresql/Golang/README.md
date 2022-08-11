# Antlr v4 Postgresql Golang

Steps for usage:

# Step 1.

Update the lexical file and grammar file to the latest version, the latest version can be obtained from this place:  
[https://github.com/antlr/grammars-v4/tree/master/sql/postgresql](https://github.com/antlr/grammars-v4/tree/master/sql/postgresql)

# Step 2.

First run `main.go` in the project root directory, convert `PostgreSQLLexer.g4` and `PostgreSQLParser.g4` into a form
suitable for `Golang`, the converted directory is stored in other directories, and the default is stored in the project
root directory Golang directory under .

### Why do this step of conversion?

The original g4 file is compatible with Java and C#, but some features of Java and C# cannot be adapted in Golang. For
example, in the member method of a class in Java, this can be omitted to call the same class or parent class. method,
but not in Golang, you must manually specify the receiver, and other problems such as package paths in golang are
different from other languages, but you don't want to change the original g4 file, because once Changing g4 cannot
guarantee the correctness of Java and C#. It is very stressful to ensure support for so many languages ine the single g4
file, so I chose to perform it based on the characteristics of golang based on the original file. Tweaked the generated
new file so that it could adapt to Golang's syntax

# Step 3.

Then use `antlr-4.10.1-complete.jar` to generate the corresponding code from the converted lexical file and grammar
file. The name of the jar package may be different in different versions, please replace it with your own version.

Generate lexical parsing code:

```bash
java -jar ./antlr-4.10.1-complete.jar -Dlanguage=Go ./Golang/PostgreSQLLexer.g4 -o parser -visitor
````

Generate parsing code:

````
java -jar ./antlr-4.10.1-complete.jar -Dlanguage=Go ./Golang/PostgreSQLParser.g4 -o parser -visitor
````

# Step 4.

Run `test/main.go` to see if Kangkang can output normally.

# Don't want to do it yourself, have you done it?

I have a compiled project:

````text
https://github.com/CC11001100/go-antlrv4-postgresql-parser
````

You can continue based on this project, or add dependencies directly:

```shell
go get -u github.com/CC11001100/go-antlrv4-postgresql-parser
````

