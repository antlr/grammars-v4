#Rego Grammar

Dual licensed under 3-Clause BSD, and MIT.

Copyright (C) Arroyo Networks

Authors: Josh Marshall (j.marshall@arroyo.io)

##About

##Building

See Antlr4 main documentation

##Developing

###Testing

`mvn test`
The `-e` and `-X` flags to `mvn` are frequently helpful.

##Notes

Test files are in `examples/` and end with the '.stmt' suffix.  Tests which start with 'e' are error cases.  Any error case which can be caught by the grammar will have an accompanying file which ends with '.errors'.  If an error case does not have an accompanying '.errors. file, that means it must be handled by the interpreter.
