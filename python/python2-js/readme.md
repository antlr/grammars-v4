### Create javascript Source

```shell
alias antlr4='java -Xmx500M -cp "/usr/local/lib/antlr-4.7-complete.jar:$CLASSPATH" org.antlr.v4.Tool'

antlr4 -Dlanguage=JavaScript -no-listener -no-visitor Python2.g4
```

### example
```shell
node test.js
// line 7:21 no viable alternative at input 'printme("hello world";'
```
