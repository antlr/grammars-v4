# XQuery 4.0 vs XPath 4.0 EBNF Comparison

Sources:
- XQuery 4.0: https://qt4cg.org/specifications/grammar-explorer/xquery40/index.html
- XPath 4.0:  https://qt4cg.org/specifications/grammar-explorer/xpath40/index.html

**Summary:** XQuery has 325 rules, XPath has 213. 195 rules are identical in both. 13 rules exist in both but differ. 117 rules exist only in XQuery, 5 only in XPath.

Note on `*`, `++`, `**`, `?` operators: Neither grammar uses `++` or `**` as operators. The only special compound is `+:=` (RecordPutExpr), which is **identical** in both. The `*`, `+`, and `?` quantifiers were extracted from CSS class names (`zeroOrMore`, `oneOrMore`, `optional`) in the HTML source — all quantifier differences are reflected in the 13 differing rules below.

---

## Rules in BOTH but DIFFERENT (13)

### 1. `AposStringLiteral`
```
XQuery: ("'" (PredefinedEntityRef | CharRef | EscapeApos | ^['&])* "'")
XPath:  ("'" (EscapeApos | ^['])* "'")
```
XQuery additionally handles `PredefinedEntityRef` and `CharRef` (XML entity/character references) and also excludes `&` from the bare-character class.

### 2. `Axis`
```
XQuery: ("ancestor" | "ancestor-or-self" | "attribute" | "child" | "descendant" |
         "descendant-or-self" | "following" | "following-or-self" | "following-sibling" |
         "following-sibling-or-self" | "parent" | "preceding" | "preceding-or-self" |
         "preceding-sibling" | "preceding-sibling-or-self" | "self") "::"

XPath:  ("ancestor" | "ancestor-or-self" | "attribute" | "child" | "descendant" |
         "descendant-or-self" | "following" | "following-or-self" | "following-sibling" |
         "following-sibling-or-self" | "namespace" | "parent" | "preceding" |
         "preceding-or-self" | "preceding-sibling" | "preceding-sibling-or-self" | "self") "::"
```
XPath adds the **`"namespace"`** axis. XQuery omits it.

### 3. `BracedURILiteral`
```
XQuery: "Q" "{" (PredefinedEntityRef | CharRef | ^[&{}])* "}"
XPath:  "Q" "{" (^[{}])* "}"
```
XQuery additionally allows `PredefinedEntityRef` and `CharRef` inside braced URIs and excludes `&` from bare characters. XPath only excludes `{` and `}`.

### 4. `CompNodeNCName`
```
XQuery: (MarkedNCName | UnreservedNCName | ("{" Expr "}"))
XPath:  (MarkedNCName | ("{" Expr "}"))
```
XQuery adds `UnreservedNCName` as an alternative.

### 5. `CompNodeName`
```
XQuery: (QNameLiteral | UnreservedName | ("{" Expr "}"))
XPath:  (QNameLiteral | ("{" Expr "}"))
```
XQuery adds `UnreservedName` as an alternative.

### 6. `ExprSingle`
```
XQuery: (FLWORExpr | QuantifiedExpr | SwitchExpr | TypeswitchExpr | IfExpr | TryCatchExpr | OrExpr)
XPath:  (ForExpr | LetExpr | QuantifiedExpr | IfExpr | OrExpr)
```
XQuery has full FLWOR (`FLWORExpr`), `SwitchExpr`, `TypeswitchExpr`, `TryCatchExpr`. XPath uses simpler recursive `ForExpr`/`LetExpr` instead.

### 7. `ForItemBinding`
```
XQuery: VarNameAndType AllowingEmpty? PositionalVar? "in" ExprSingle
XPath:  VarNameAndType PositionalVar? "in" ExprSingle
```
XQuery adds optional `AllowingEmpty?` (to handle empty sequences in outer joins).

### 8. `FunctionType`
```
XQuery: Annotation* (AnyFunctionType | TypedFunctionType)
XPath:  (AnyFunctionType | TypedFunctionType)
```
XQuery allows zero or more `Annotation*` before a function type.

### 9. `InlineFunctionExpr`
```
XQuery: Annotation* ("function" | "fn") FunctionSignature? FunctionBody
XPath:  ("function" | "fn") FunctionSignature? FunctionBody
```
XQuery allows zero or more `Annotation*` before inline functions.

### 10. `NodeConstructor`
```
XQuery: (DirectConstructor | ComputedConstructor)
XPath:  (ComputedConstructor)
```
XQuery adds `DirectConstructor` (literal XML syntax like `<elem/>`). XPath only has computed constructors.

### 11. `PrimaryExpr`
```
XQuery: (Literal | VarRef | ParenthesizedExpr | ContextValueRef | FunctionCall |
         OrderedExpr | UnorderedExpr | NodeConstructor | FunctionItemExpr |
         MapConstructor | ArrayConstructor | StringTemplate | StringConstructor | UnaryLookup)

XPath:  (Literal | VarRef | ParenthesizedExpr | ContextValueRef | FunctionCall |
         NodeConstructor | FunctionItemExpr | MapConstructor | ArrayConstructor |
         StringTemplate | UnaryLookup)
```
XQuery adds `OrderedExpr`, `UnorderedExpr`, and `StringConstructor` (the `` ``[...]`` `` form).

### 12. `QuotStringLiteral`
```
XQuery: (""" (PredefinedEntityRef | CharRef | EscapeQuot | ^["&])* """)
XPath:  (""" (EscapeQuot | ^["])* """)
```
Same pattern as `AposStringLiteral`: XQuery allows XML entity/character references and excludes `&` from bare characters.

### 13. `ValueExpr`
```
XQuery: (ValidateExpr | ExtensionExpr | SimpleMapExpr)
XPath:  (SimpleMapExpr)
```
XQuery adds `ValidateExpr` (schema validation) and `ExtensionExpr` (pragma extension).

---

## Rules ONLY in XPath (5)

| Rule | Definition |
|------|-----------|
| `XPath` | `(DefaultElementNamespaceDecl ";")? (NamespaceDecl ";")* Expr` — top-level entry point |
| `DefaultElementNamespaceDecl` | `"declare" "default" "element" "namespace" URILiteral` |
| `ForExpr` | `ForClause ForLetReturn` |
| `LetExpr` | `LetClause ForLetReturn` |
| `ForLetReturn` | `(ForExpr | LetExpr | ("return" ExprSingle))` |

XPath uses a right-recursive structure for for/let expressions (`ForLetReturn`) rather than XQuery's general `FLWORExpr` with `IntermediateClause*`.

---

## Rules ONLY in XQuery (117)

### Module/Prolog structure
`QueryList`, `Module`, `VersionDecl`, `LibraryModule`, `MainModule`, `ModuleDecl`, `ModuleImport`, `Prolog`, `QueryBody`, `Separator`, `Import`

### Declarations/Setters
`Setter`, `BoundarySpaceDecl`, `DefaultCollationDecl`, `BaseURIDecl`, `ConstructionDecl`, `OrderingModeDecl`, `EmptyOrderDecl`, `CopyNamespacesDecl`, `InheritMode`, `PreserveMode`, `DecimalFormatDecl`, `DFPropertyName`, `DefaultNamespaceDecl`, `NamedRecordTypeDecl`, `ItemTypeDecl`, `OptionDecl`, `VarDecl`, `VarValue`, `VarDefaultValue`, `FunctionDecl`, `ContextValueDecl`

### Schema/Validation
`SchemaImport`, `SchemaPrefix`, `ValidateExpr`, `ValidationMode`

### Annotations
`Annotation`

### FLWOR-specific
`FLWORExpr`, `InitialClause`, `IntermediateClause`, `ReturnClause`, `AllowingEmpty`, `ForBinding`, `WhereClause`, `WhileClause`, `GroupByClause`, `GroupingSpec`, `OrderByClause`, `OrderSpec`, `OrderModifier`, `CountClause`, `TraceClause`, `WindowClause`, `TumblingWindowClause`, `SlidingWindowClause`, `WindowStartCondition`, `WindowEndCondition`, `WindowVars`, `CurrentVar`, `PreviousVar`, `NextVar`

### Switch/Typeswitch
`SwitchExpr`, `SwitchComparand`, `SwitchCases`, `BracedSwitchCases`, `SwitchCaseClause`, `SwitchCaseOperand`, `TypeswitchExpr`, `TypeswitchCases`, `BracedTypeswitchCases`, `CaseClause`, `SequenceTypeUnion`

### Try/Catch
`TryCatchExpr`, `TryClause`, `CatchClause`, `FinallyClause`

### Direct XML constructors
`DirectConstructor`, `DirElemConstructor`, `DirAttributeList`, `DirAttributeValue`, `DirElemContent`, `DirCommentConstructor`, `DirCommentContents`, `DirCommentContentDoubleDashError`, `DirPIConstructor`, `DirPIContents`, `PITarget`, `CDataSection`, `CDataSectionContents`, `ElementContentChar`, `AposAttrContentChar`, `AposAttrValueContent`, `QuotAttrContentChar`, `QuotAttrValueContent`, `CommonContent`, `OpenApos`, `OpenQuot`, `CloseApos`, `CloseQuot`, `LCurlyBraceEscape`, `RCurlyBraceEscape`

### Extensions
`ExtensionExpr`, `Pragma`, `PragmaContents`

### Ordered/Unordered expressions
`OrderedExpr`, `UnorderedExpr`

### String constructor
`StringConstructor`, `StringConstructorContent`, `StringConstructorChars`, `StringInterpolation`

### Function parameters with defaults
`ParamListWithDefaults`, `ParamWithDefault`

### XML entity/character references
`PredefinedEntityRef`, `CharRef`

### Unreserved name aliases
`UnreservedName`, `UnreservedNCName`

### Miscellaneous
`S`, `ExtendedFieldDeclaration`
