// moduleDecl -> decorator* "module" IDENTIFIER(name) interpString(type) "=" (ifCondition | object | forExpression) NL

module x '...' = {}

@dec(42)
module x '...' = {
  x: y
  mu: 'foo'
}
