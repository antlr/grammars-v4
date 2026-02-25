import sys
import re
from antlr4 import Parser
from SymbolTable import SymbolTable
from Symbol import Symbol
from TypeClassification import TypeClassification

ALL_SEMANTIC_FUNCTIONS = [
    "IsAlignmentSpecifier", "IsAtomicTypeSpecifier", "IsAttributeDeclaration",
    "IsAttributeSpecifier", "IsAttributeSpecifierSequence", "IsDeclaration",
    "IsDeclarationSpecifier", "IsTypeSpecifierQualifier", "IsEnumSpecifier",
    "IsFunctionSpecifier", "IsStatement", "IsStaticAssertDeclaration",
    "IsStorageClassSpecifier", "IsStructOrUnionSpecifier", "IsTypedefName",
    "IsTypeofSpecifier", "IsTypeQualifier", "IsTypeSpecifier", "IsCast",
    "IsNullStructDeclarationListExtension",
    "IsGnuAttributeBeforeDeclarator",
    "IsSomethingOfTypeName", "IsSpecifierQualifierList", "IsTypeName",
    "IsInitDeclaratorList"
]

def parseNoSemantics(args):
    result = set()
    for a in args:
        lower = a.lower()
        if lower.startswith("--no-semantics"):
            eqIndex = a.find('=')
            if eqIndex == -1:
                for func in ALL_SEMANTIC_FUNCTIONS:
                    result.add(func)
            else:
                value = a[eqIndex + 1:]
                funcs = value.split(',')
                for func in funcs:
                    result.add(func.strip())
    return result

class CParserBase(Parser):
    def __init__(self, input, output=sys.stdout):
        super().__init__(input, output)
        args = sys.argv
        self.noSemantics = parseNoSemantics(args)
        self._debug = any("--debug" in a.lower() for a in args)
        self.outputSymbolTable = any("--output-symbol-table" in a.lower() for a in args)
        self.outputAppliedOccurrences = any("--output-applied-occurrences" in a.lower() for a in args)
        self._st = SymbolTable()
        self._CLexer = None
        self._CParser = None

    def _getLexerModule(self):
        if self._CLexer is None:
            from CLexer import CLexer
            self._CLexer = CLexer
        return self._CLexer

    def _getParserModule(self):
        if self._CParser is None:
            from CParser import CParser
            self._CParser = CParser
        return self._CParser

    def IsAlignmentSpecifier(self, k=1):
        if "IsAlignmentSpecifier" in self.noSemantics:
            return True
        lt1 = self._input.LT(k)
        text = lt1.text
        if self._debug:
            sys.stdout.write("IsAlignmentSpecifier " + str(lt1))
        resolved = self._resolveWithOutput(lt1)
        result = False
        if resolved is None:
            result = False
        elif TypeClassification.AlignmentSpecifier_ in resolved.classification:
            result = True
        else:
            result = False
        if self._debug:
            print(" " + str(result))
        return result

    def IsAtomicTypeSpecifier(self, k=1):
        if "IsAtomicTypeSpecifier" in self.noSemantics:
            return True
        lt1 = self._input.LT(k)
        text = lt1.text
        if self._debug:
            sys.stdout.write("IsAtomicTypeSpecifier " + str(lt1))
        resolved = self._resolveWithOutput(lt1)
        result = False
        if resolved is None:
            result = False
        elif TypeClassification.AtomicTypeSpecifier_ in resolved.classification:
            result = True
        else:
            result = False
        if self._debug:
            print(" " + str(result))
        return result

    def IsAttributeDeclaration(self):
        if "IsAttributeDeclaration" in self.noSemantics:
            return True
        return self.IsAttributeSpecifierSequence()

    def IsAttributeSpecifier(self):
        if "IsAttributeSpecifier" in self.noSemantics:
            return True
        CLexer = self._getLexerModule()
        lt1 = self._input.LT(1)
        if self._debug:
            sys.stdout.write("IsAttributeSpecifier " + str(lt1))
        result = lt1.type == CLexer.LeftBracket
        if self._debug:
            print(" " + str(result))
        return result

    def IsAttributeSpecifierSequence(self):
        if "IsAttributeSpecifierSequence" in self.noSemantics:
            return True
        return self.IsAttributeSpecifier()

    def IsDeclaration(self):
        if "IsDeclaration" in self.noSemantics:
            return True
        if self._debug:
            print("IsDeclaration")
        result = (self.IsDeclarationSpecifiers()
            or self.IsAttributeSpecifierSequence()
            or self.IsStaticAssertDeclaration()
            or self.IsAttributeDeclaration())
        if self._debug:
            print("IsDeclaration " + str(result))
        return result

    def IsDeclarationSpecifier(self):
        if "IsDeclarationSpecifier" in self.noSemantics:
            return True
        lt1 = self._input.LT(1)
        text = lt1.text
        if self._debug:
            print("IsDeclarationSpecifier " + str(lt1))
        result = (self.IsStorageClassSpecifier()
            or self.IsTypeSpecifier()
            or self.IsTypeQualifier()
            or (self.IsFunctionSpecifier() and not self.IsGnuAttributeBeforeDeclarator())
            or self.IsAlignmentSpecifier())
        if self._debug:
            print("IsDeclarationSpecifier " + str(result) + " for " + str(lt1))
        return result

    def IsTypeSpecifierQualifier(self, k=1):
        if "IsTypeSpecifierQualifier" in self.noSemantics:
            return True
        if self._debug:
            print("IsDeclarationSpecifier")
        result = (self.IsTypeSpecifier(k)
            or self.IsTypeQualifier(k)
            or self.IsAlignmentSpecifier(k))
        if self._debug:
            print("IsDeclarationSpecifier " + str(result))
        return result

    def IsDeclarationSpecifiers(self):
        if "IsDeclarationSpecifiers" in self.noSemantics:
            return True
        return self.IsDeclarationSpecifier()

    def IsEnumSpecifier(self, k=1):
        if "IsEnumSpecifier" in self.noSemantics:
            return True
        CLexer = self._getLexerModule()
        lt1 = self._input.LT(k)
        if self._debug:
            sys.stdout.write("IsEnumSpecifier " + str(lt1))
        result = lt1.type == CLexer.Enum
        if self._debug:
            print(" " + str(result))
        return result

    def IsFunctionSpecifier(self):
        if "IsFunctionSpecifier" in self.noSemantics:
            return True
        lt1 = self._input.LT(1)
        text = lt1.text
        if self._debug:
            sys.stdout.write("IsFunctionSpecifier " + str(lt1))
        resolved = self._resolveWithOutput(lt1)
        result = False
        if resolved is None:
            result = False
        elif TypeClassification.FunctionSpecifier_ in resolved.classification:
            result = True
        else:
            result = False
        if self._debug:
            print("IsFunctionSpecifier " + str(result))
        return result

    def IsGnuAttributeBeforeDeclarator(self, k=1):
        if "IsGnuAttributeBeforeDeclarator" in self.noSemantics:
            return True
        CLexer = self._getLexerModule()
        i = k
        if self._input.LT(i).type != CLexer.Attribute:
            return False
        i += 1
        depth = 0
        while True:
            t = self._input.LT(i)
            i += 1
            if t.type < 0:  # EOF
                return False
            if t.type == CLexer.LeftParen:
                depth += 1
            elif t.type == CLexer.RightParen:
                depth -= 1
                if depth == 0:
                    break
        next_type = self._input.LT(i).type
        return (next_type == CLexer.Identifier
                or next_type == CLexer.Star
                or next_type == CLexer.LeftParen)

    def IsStatement(self):
        if "IsStatement" in self.noSemantics:
            return True
        CLexer = self._getLexerModule()
        t1 = self._input.LT(1)
        t2 = self._input.LT(2)
        if self._debug:
            print("IsStatement1 " + str(t1))
        if self._debug:
            print("IsStatement2 " + str(t2))
        if t1.type == CLexer.Identifier and t2.type == CLexer.Colon:
            if self._debug:
                sys.stdout.write("IsStatement3 true")
            return True
        result = not self.IsDeclaration()
        if self._debug:
            sys.stdout.write("IsStatement " + str(result))
        return result

    def IsStaticAssertDeclaration(self):
        if "IsStaticAssertDeclaration" in self.noSemantics:
            return True
        CLexer = self._getLexerModule()
        token = self._input.LT(1)
        if self._debug:
            sys.stdout.write("IsStaticAssertDeclaration " + str(token))
        result = token.type == CLexer.Static_assert
        if self._debug:
            print(" " + str(result))
        return result

    def IsStorageClassSpecifier(self):
        if "IsStorageClassSpecifier" in self.noSemantics:
            return True
        lt1 = self._input.LT(1)
        text = lt1.text
        if self._debug:
            sys.stdout.write("IsStorageClassSpecifier " + str(lt1))
        resolved = self._resolveWithOutput(lt1)
        result = False
        if resolved is None:
            result = False
        elif TypeClassification.StorageClassSpecifier_ in resolved.classification:
            result = True
        else:
            result = False
        if self._debug:
            print(" " + str(result))
        return result

    def IsStructOrUnionSpecifier(self, k=1):
        if "IsStructOrUnionSpecifier" in self.noSemantics:
            return True
        CLexer = self._getLexerModule()
        token = self._input.LT(k)
        if self._debug:
            sys.stdout.write("IsStructOrUnionSpecifier " + str(token))
        result = token.type == CLexer.Struct or token.type == CLexer.Union
        if self._debug:
            print(" " + str(result))
        return result

    def IsTypedefName(self, k=1):
        if "IsTypedefName" in self.noSemantics:
            return True
        lt1 = self._input.LT(k)
        text = lt1.text
        if self._debug:
            sys.stdout.write("IsTypedefName " + str(lt1))
        resolved = self._resolveWithOutput(lt1)
        result = False
        if resolved is None:
            result = False
        elif TypeClassification.Variable_ in resolved.classification:
            result = False
        elif TypeClassification.Function_ in resolved.classification:
            result = False
        else:
            result = True
        if self._debug:
            print(" " + str(result))
        return result

    def IsTypeofSpecifier(self, k=1):
        if "IsTypeofSpecifier" in self.noSemantics:
            return True
        CLexer = self._getLexerModule()
        token = self._input.LT(k)
        if self._debug:
            sys.stdout.write("IsTypeofSpecifier " + str(token))
        result = token.type == CLexer.Typeof or token.type == CLexer.Typeof_unqual
        if self._debug:
            print(" " + str(result))
        return result

    def IsTypeQualifier(self, k=1):
        if "IsTypeQualifier" in self.noSemantics:
            return True
        lt1 = self._input.LT(k)
        text = lt1.text
        if self._debug:
            sys.stdout.write("IsTypeQualifier " + str(lt1))
        resolved = self._resolveWithOutput(lt1)
        result = False
        if resolved is None:
            result = False
        elif TypeClassification.TypeQualifier_ in resolved.classification:
            result = True
        else:
            result = False
        if self._debug:
            print(" " + str(result))
        return result

    def IsTypeSpecifier(self, k=1):
        if "IsTypeSpecifier" in self.noSemantics:
            return True
        lt1 = self._input.LT(k)
        text = lt1.text
        if self._debug:
            sys.stdout.write("IsTypeSpecifier " + str(lt1))
        resolved = self._resolveWithOutput(lt1)
        result = False
        if resolved is None:
            result = False
        elif TypeClassification.TypeSpecifier_ in resolved.classification:
            result = True
        else:
            result = False

        if result:
            if self._debug:
                print(" " + str(result))
            return result
        result = (self.IsAtomicTypeSpecifier(k) or self.IsStructOrUnionSpecifier(k) or self.IsEnumSpecifier(k)
            or self.IsTypedefName(k) or self.IsTypeofSpecifier(k))
        if self._debug:
            print(" " + str(result))
        return result

    def _myGetDeclarationId(self, y):
        token = self._myGetDeclarationToken(y)
        return token.text if token is not None else None

    def _myGetDeclarationToken(self, y):
        if y is None:
            return None

        directDeclarator = y.directDeclarator()
        if directDeclarator is not None:
            more = directDeclarator.declarator()
            token = self._myGetDeclarationToken(more)
            if token is not None:
                return token
            if directDeclarator.Identifier() is not None:
                return directDeclarator.Identifier().symbol

        return None

    def EnterDeclaration(self):
        if self._debug:
            print("EnterDeclaration")
        CParser = self._getParserModule()
        CLexer = self._getLexerModule()
        context = self._ctx
        while context is not None:
            if isinstance(context, CParser.DeclarationContext):
                if not hasattr(context, 'getRuleIndex') or context.getRuleIndex() != CParser.RULE_declaration:
                    context = context.parentCtx
                    continue
                declaration_context = context
                declaration_specifiers = declaration_context.declarationSpecifiers()
                declaration_specifier = None
                if declaration_specifiers is not None:
                    declaration_specifier = declaration_specifiers.declarationSpecifier()
                if declaration_specifier is None:
                    declaration_specifier = None

                is_typedef = False
                if declaration_specifier is not None:
                    for ds in declaration_specifier:
                        scs = ds.storageClassSpecifier()
                        if scs is not None and scs.Typedef() is not None:
                            is_typedef = True
                            break
                    for ds in declaration_specifier:
                        scs = ds.storageClassSpecifier()
                        if scs is not None and scs.Typedef() is not None:
                            is_typedef = True
                            break
                    for ds in declaration_specifier:
                        scs = ds.storageClassSpecifier()
                        if scs is not None and scs.Typedef() is not None:
                            is_typedef = True
                            break

                init_declaration_list = declaration_context.initDeclaratorList()
                init_declarators = None
                if init_declaration_list is not None:
                    init_declarators = init_declaration_list.initDeclarator()
                if init_declarators is None:
                    init_declarators = None

                if init_declarators is not None:
                    for id_ in init_declarators:
                        y = id_.declarator() if id_ is not None else None
                        idToken = self._myGetDeclarationToken(y)
                        if idToken is not None:
                            text = idToken.text
                            loc = self._getSourceLocation(idToken)
                            if is_typedef:
                                symbol = Symbol()
                                symbol.name = text
                                symbol.classification = {TypeClassification.TypeSpecifier_}
                                symbol.definedFile = loc["file"]
                                symbol.definedLine = loc["line"]
                                symbol.definedColumn = loc["column"]
                                self._st.define(symbol)
                                if self._debug:
                                    print("New symbol Declaration2 Declarator " + str(symbol))
                            else:
                                symbol = Symbol()
                                symbol.name = text
                                symbol.classification = {TypeClassification.Variable_}
                                symbol.definedFile = loc["file"]
                                symbol.definedLine = loc["line"]
                                symbol.definedColumn = loc["column"]
                                self._st.define(symbol)
                                if self._debug:
                                    print("New symbol Declaration3 Declarator " + str(symbol))

            if isinstance(context, CParser.FunctionDefinitionContext):
                if not hasattr(context, 'getRuleIndex') or context.getRuleIndex() != CParser.RULE_functionDefinition:
                    context = context.parentCtx
                    continue
                fd = context
                de = fd.declarator()
                if de is None:
                    context = context.parentCtx
                    continue
                dd = de.directDeclarator()
                if dd is None:
                    context = context.parentCtx
                    continue
                if dd is not None and dd.Identifier() is not None:
                    idToken = dd.Identifier().symbol
                    text = idToken.text
                    loc = self._getSourceLocation(idToken)
                    symbol = Symbol()
                    symbol.name = text
                    symbol.classification = {TypeClassification.Function_}
                    symbol.definedFile = loc["file"]
                    symbol.definedLine = loc["line"]
                    symbol.definedColumn = loc["column"]
                    self._st.define(symbol)
                    if self._debug:
                        print("New symbol Declarationf Declarator " + str(symbol))
                    return

            context = context.parentCtx

    def IsNullStructDeclarationListExtension(self):
        if "IsNullStructDeclarationListExtension" in self.noSemantics:
            return True
        return True

    def EnterScope(self):
        if self._debug:
            print("EnterScope")
        self._st.pushBlockScope()

    def ExitScope(self):
        if self._debug:
            print("ExitScope")
        self._st.popBlockScope()

    def LookupSymbol(self):
        token = self._input.LT(-1)
        if token is None:
            return
        text = token.text
        resolved = self._st.resolve(text)
        if self.outputAppliedOccurrences and resolved is not None:
            appliedLoc = self._getSourceLocation(token)
            sys.stderr.write(f"Applied occurrence: {text} at {appliedLoc['file']}:{appliedLoc['line']}:{appliedLoc['column']} -> Defined at {resolved.definedFile}:{resolved.definedLine}:{resolved.definedColumn}\n")

    def OutputSymbolTable(self):
        if self.outputSymbolTable:
            sys.stderr.write(str(self._st) + "\n")

    def _resolveWithOutput(self, token):
        if token is None:
            return None
        text = token.text
        resolved = self._st.resolve(text)
        if self.outputAppliedOccurrences and resolved is not None:
            appliedLoc = self._getSourceLocation(token)
            sys.stderr.write(f"Applied occurrence: {text} at {appliedLoc['file']}:{appliedLoc['line']}:{appliedLoc['column']} -> Defined at {resolved.definedFile}:{resolved.definedLine}:{resolved.definedColumn}\n")
        return resolved

    def _getSourceLocation(self, token):
        if token is None:
            return {"file": "", "line": 0, "column": 0}

        CLexer = self._getLexerModule()
        fileName = "<unknown>"
        line = token.line
        column = token.column
        lineAdjusted = line

        ts = self._input
        ind = token.tokenIndex

        j = ind
        while j >= 0:
            t = ts.get(j)
            if t is None:
                break
            if t.type == CLexer.LineDirective:
                txt = t.text
                parts = re.split(r'\s+', txt)
                if len(parts) >= 3:
                    try:
                        dirLine = int(parts[1])
                        lineDirective = t.line
                        lineDiff = line - lineDirective
                        lineAdjusted = lineDiff + dirLine - 1
                        fileName = parts[2].strip()
                        if fileName.startswith('"') and fileName.endswith('"'):
                            fileName = fileName[1:-1]
                    except ValueError:
                        pass
                break
            j -= 1

        return {"file": fileName, "line": lineAdjusted, "column": column}

    def IsInitDeclaratorList(self):
        # Cannot be initDeclaratorList if the first thing is a type.
        # Types need to go to preceding declarationSpecifiers.
        if "IsInitDeclaratorList" in self.noSemantics:
            return True
        lt1 = self._input.LT(1)
        text = lt1.text
        if self._debug:
            sys.stdout.write("IsInitDeclaratorList " + str(lt1))
        resolved = self._resolveWithOutput(lt1)
        result = False
        if resolved is None:
            result = True
        elif TypeClassification.TypeQualifier_ in resolved.classification or TypeClassification.TypeSpecifier_ in resolved.classification:
            result = False
        else:
            result = True
        if self._debug:
            print(" " + str(result))
        return result

    def IsSomethingOfTypeName(self):
        if "IsSomethingOfTypeName" in self.noSemantics:
            return True
        CLexer = self._getLexerModule()
        if not (self._input.LT(1).type == CLexer.Sizeof or
                self._input.LT(1).type == CLexer.Countof or
                self._input.LT(1).type == CLexer.Alignof or
                self._input.LT(1).type == CLexer.Maxof or
                self._input.LT(1).type == CLexer.Minof):
            return False
        if self._input.LT(2).type != CLexer.LeftParen:
            return False
        if self.IsTypeName(3):
            return True
        return False

    def IsTypeName(self, k=1):
        if "IsTypeName" in self.noSemantics:
            return True
        return self.IsSpecifierQualifierList(k)

    def IsSpecifierQualifierList(self, k=1):
        if "IsSpecifierQualifierList" in self.noSemantics:
            return True
        if self.IsGnuAttributeBeforeDeclarator(k):
            return True
        if self.IsTypeSpecifierQualifier(k):
            return True
        return False

    def IsCast(self):
        result = False
        if "IsCast" in self.noSemantics:
            return True
        CLexer = self._getLexerModule()
        t1 = self._input.LT(1)
        t2 = self._input.LT(2)
        if self._debug:
            print("IsCast1 " + str(t1))
        if self._debug:
            print("IsCast2 " + str(t2))
        if t1.type != CLexer.LeftParen:
            if self._debug:
                sys.stdout.write("IsCast " + str(result))
        elif t2.type != CLexer.Identifier:
            result = True
        else:
            resolved = self._resolveWithOutput(t2)
            if resolved is None:
                result = False
            elif TypeClassification.TypeSpecifier_ in resolved.classification:
                result = True
            else:
                result = False
        if self._debug:
            sys.stdout.write("IsStatement " + str(result))
        return result
