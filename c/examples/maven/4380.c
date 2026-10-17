// This example illustrates ambiguity in the chose of a statement vs.
// declaration in blockItem. The parse cannot be a statement if it
// begins with a type.
// ../examples/4380.c.d=116.a=1: (compilationUnit (translationUnit (externalDeclaration (declaration (declarationSpecifiers (declarationSpecifier (typeSpecifier (structOrUnionSpecifier (structOrUnion (Struct "struct")) (Identifier "nginx") (LeftBrace "{") (RightBrace "}"))))) (Semi ";"))) (externalDeclaration (functionDefinition (declarationSpecifiers (declarationSpecifier (typeSpecifier (Int "int")))) (declarator (directDeclarator (directDeclarator (Identifier "main")) (LeftParen "(") (RightParen ")"))) (compoundStatement (LeftBrace "{") (blockItemList (blockItem (statement (expressionStatement (expression (assignmentExpression (conditionalExpression (logicalOrExpression (logicalAndExpression (inclusiveOrExpression (exclusiveOrExpression (andExpression (equalityExpression (relationalExpression (shiftExpression (additiveExpression (multiplicativeExpression (castExpression (unaryExpression (postfixExpression (primaryExpression (Identifier "nginx"))))) (Star "*") (castExpression (unaryExpression (postfixExpression (primaryExpression (Identifier "d")))))))))))))))))) (Semi ";"))))) (RightBrace "}"))))) (EOF ""))
// ../examples/4380.c.d=116.a=2: (compilationUnit (translationUnit (externalDeclaration (declaration (declarationSpecifiers (declarationSpecifier (typeSpecifier (structOrUnionSpecifier (structOrUnion (Struct "struct")) (Identifier "nginx") (LeftBrace "{") (RightBrace "}"))))) (Semi ";"))) (externalDeclaration (functionDefinition (declarationSpecifiers (declarationSpecifier (typeSpecifier (Int "int")))) (declarator (directDeclarator (directDeclarator (Identifier "main")) (LeftParen "(") (RightParen ")"))) (compoundStatement (LeftBrace "{") (blockItemList (blockItem (declaration (declarationSpecifiers (declarationSpecifier (typeSpecifier (typedefName (Identifier "nginx"))))) (initDeclaratorList (initDeclarator (declarator (pointer (Star "*")) (directDeclarator (Identifier "d"))))) (Semi ";")))) (RightBrace "}"))))) (EOF ""))
// See https://groups.google.com/g/antlr-discussion/c/nGvUxmnOxsI

struct nginx {};

int main() {
	nginx *d;
}
