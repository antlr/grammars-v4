Sgf.g4
======
This is an antlr4-grammar for the `Smart Game Format`
which is also called `Smart Go Format`.

Sgf is a context dependend format, which means a standard parser is
of limited use. The format however is pretty simple and this parser
gives a very good starting point.

All properties from the [list of property](http://www.red-bean.com/sgf/proplist.html)
are implemented and the official [example](http://www.red-bean.com/sgf/examples/) parses without problems.

Unknown properties are supported and will show up as `privateProp`-nodes. 

Definition
----------

* http://www.red-bean.com/sgf/sgf4.htm
* http://www.red-bean.com/sgf/properties.htm
* http://www.red-bean.com/sgf/proplist.html
* http://www.red-bean.com/sgf/go.htm (Go)

License
-------
[AGPL-3.0](agpl-3.0.txt)
