/*
  importDecl -> decorator* "import" interpString(specification) importWithClause? importAsClause? NL
  importWithClause -> "with" object
  importAsClause -> "as" IDENTIFIER(alias)
*/

import 'something'

import 'something' with {
  Q: 42
  Fu: 'BAR'
}

import 'something' with {
  Q: 42
} as X

@decorator(var)
import 'something.${1 + 2}' with {
  Q: 42
  Fu: 'BAR'
} as X
