# Python2 JavaScript Grammar

## Create JavaScript Source

```shell
alias antlr4='java -Xmx500M -cp "/usr/local/lib/antlr-4.7-complete.jar:$CLASSPATH" org.antlr.v4.Tool'

antlr4 -Dlanguage=JavaScript -no-listener -no-visitor Python2.g4
```

## Example
```shell
node test.js
// line 7:21 no viable alternative at input 'printme("hello world";'
```

## Reference
* [pldb](http://pldb.info/concepts/python)

