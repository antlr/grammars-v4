from antlr4 import *
from antlr4.Token import CommonToken
import sys
import os
from typing import TextIO


class AdaParserBase(Parser):
    _package_cache = {}  # class-level: package name -> exported symbols
    _parsing_in_progress = set()  # class-level: paths being parsed

    def __init__(self, input: TokenStream, output: TextIO = sys.stdout):
        super().__init__(input, output)
        if "." in __name__:
            from .SymbolTable import SymbolTable
        else:
            from SymbolTable import SymbolTable
        self._st = SymbolTable()
        self._expected_type_stack = []
        self._debug = "--debug" in sys.argv
        self._output_symbol_table = "--output-symbol-table" in sys.argv
        self._output_applied_occurrences = "--output-applied-occurrences" in sys.argv
        self._no_semantics = self._parse_no_semantics()
        self._search_paths = []
        self._current_file = ""
        for arg in sys.argv:
            if arg.lower().startswith("--i") and len(arg) > 3:
                self._search_paths.append(arg[3:])

    @staticmethod
    def _parse_no_semantics():
        all_funcs = ["IsAggregate", "IsTypeName"]
        result = set()
        for a in sys.argv:
            if a.lower().startswith("--no-semantics"):
                eq = a.find("=")
                if eq == -1:
                    result.update(all_funcs)
                else:
                    for f in a[eq+1:].split(","):
                        result.add(f.strip())
        return result

    def IsAggregate(self):
        if "IsAggregate" in self._no_semantics:
            return True
        if "." in __name__:
            from .AdaLexer import AdaLexer
        else:
            from AdaLexer import AdaLexer
        stream = self._input
        lt1 = stream.LT(1)
        if lt1.type != AdaLexer.LP:
            return False
        depth = 0
        i = 2
        while True:
            t = stream.LT(i)
            if t is None or t.type == Token.EOF:
                break
            if t.type == AdaLexer.LP:
                depth += 1
            elif t.type == AdaLexer.RP:
                if depth == 0:
                    break
                depth -= 1
            elif depth == 0:
                if t.type == AdaLexer.COMMA:
                    return True
                if t.type == AdaLexer.ARROW:
                    return True
                if t.type == AdaLexer.WITH:
                    return True
                if t.type == AdaLexer.NULL_:
                    nxt = stream.LT(i + 1)
                    if nxt is not None and nxt.type == AdaLexer.RECORD:
                        return True
            i += 1
        if self._expected_type_stack:
            expected = self._expected_type_stack[-1]
            if expected is not None and expected.is_composite:
                return True
        return False

    def IsTypeName(self):
        if "IsTypeName" in self._no_semantics:
            return True
        if "." in __name__:
            from .AdaLexer import AdaLexer
            from .TypeClassification import TypeClassification
        else:
            from AdaLexer import AdaLexer
            from TypeClassification import TypeClassification
        stream = self._input
        lt1 = stream.LT(1)
        if lt1.type != AdaLexer.IDENTIFIER_:
            return False
        first_name = lt1.text
        resolved = self._st.resolve(first_name)
        return resolved is not None and TypeClassification.TypeName_ in resolved.classification

    def EnterDeclaration(self):
        if "." in __name__:
            from .AdaParser import AdaParser
            from .TypeClassification import TypeClassification
            from .Symbol import Symbol
        else:
            from AdaParser import AdaParser
            from TypeClassification import TypeClassification
            from Symbol import Symbol
        context = self._ctx
        while context is not None:
            if isinstance(context, AdaParser.Full_type_declarationContext):
                def_id = context.defining_identifier()
                if def_id:
                    is_composite = False
                    type_def = context.type_definition()
                    if type_def:
                        is_composite = (type_def.record_type_definition() is not None or
                                       type_def.array_type_definition() is not None)
                    self._define_symbol(def_id.getText(), TypeClassification.TypeName_, def_id.start, is_composite)
                return
            if isinstance(context, AdaParser.Subtype_declarationContext):
                def_id = context.defining_identifier()
                if def_id:
                    is_composite = False
                    si = context.subtype_indication()
                    if si and si.subtype_mark():
                        base_sym = self._st.resolve(si.subtype_mark().getText())
                        if base_sym:
                            is_composite = base_sym.is_composite
                    self._define_symbol(def_id.getText(), TypeClassification.TypeName_, def_id.start, is_composite)
                return
            if isinstance(context, AdaParser.Object_declarationContext):
                def_id_list = context.defining_identifier_list()
                if def_id_list:
                    for def_id in def_id_list.defining_identifier():
                        self._define_symbol(def_id.getText(), TypeClassification.ObjectName_, def_id.start)
                return
            if isinstance(context, AdaParser.Number_declarationContext):
                def_id_list = context.defining_identifier_list()
                if def_id_list:
                    for def_id in def_id_list.defining_identifier():
                        self._define_symbol(def_id.getText(), TypeClassification.ObjectName_, def_id.start)
                return
            if isinstance(context, AdaParser.Subprogram_declarationContext):
                self._define_subprogram_from_spec(context.subprogram_specification())
                return
            if isinstance(context, AdaParser.Subprogram_bodyContext):
                self._define_subprogram_from_spec(context.subprogram_specification())
                return
            if isinstance(context, AdaParser.Package_declarationContext):
                pkg_spec = context.package_specification()
                if pkg_spec:
                    dpun = pkg_spec.defining_program_unit_name()
                    if dpun and dpun.defining_identifier():
                        self._define_symbol(dpun.defining_identifier().getText(), TypeClassification.PackageName_, dpun.defining_identifier().start)
                return
            if isinstance(context, AdaParser.Package_bodyContext):
                dpun = context.defining_program_unit_name()
                if dpun and dpun.defining_identifier():
                    self._define_symbol(dpun.defining_identifier().getText(), TypeClassification.PackageName_, dpun.defining_identifier().start)
                return
            if isinstance(context, AdaParser.Exception_declarationContext):
                def_id_list = context.defining_identifier_list()
                if def_id_list:
                    for def_id in def_id_list.defining_identifier():
                        self._define_symbol(def_id.getText(), TypeClassification.ExceptionName_, def_id.start)
                return
            if isinstance(context, AdaParser.Task_type_declarationContext):
                def_id = context.defining_identifier()
                if def_id:
                    self._define_symbol(def_id.getText(), TypeClassification.TypeName_, def_id.start)
                return
            if isinstance(context, AdaParser.Single_task_declarationContext):
                def_id = context.defining_identifier()
                if def_id:
                    self._define_symbol(def_id.getText(), TypeClassification.ObjectName_, def_id.start)
                return
            if isinstance(context, AdaParser.Protected_type_declarationContext):
                def_id = context.defining_identifier()
                if def_id:
                    self._define_symbol(def_id.getText(), TypeClassification.TypeName_, def_id.start)
                return
            if isinstance(context, AdaParser.Single_protected_declarationContext):
                def_id = context.defining_identifier()
                if def_id:
                    self._define_symbol(def_id.getText(), TypeClassification.ObjectName_, def_id.start)
                return
            if isinstance(context, AdaParser.Entry_declarationContext):
                def_id = context.defining_identifier()
                if def_id:
                    self._define_symbol(def_id.getText(), TypeClassification.SubprogramName_, def_id.start)
                return
            if isinstance(context, AdaParser.Component_declarationContext):
                def_id_list = context.defining_identifier_list()
                if def_id_list:
                    for def_id in def_id_list.defining_identifier():
                        self._define_symbol(def_id.getText(), TypeClassification.ComponentName_, def_id.start)
                return
            if isinstance(context, AdaParser.Incomplete_type_declarationContext):
                def_id = context.defining_identifier()
                if def_id:
                    self._define_symbol(def_id.getText(), TypeClassification.TypeName_, def_id.start)
                return
            if isinstance(context, AdaParser.Private_type_declarationContext):
                def_id = context.defining_identifier()
                if def_id:
                    self._define_symbol(def_id.getText(), TypeClassification.TypeName_, def_id.start)
                return
            if isinstance(context, AdaParser.Private_extension_declarationContext):
                def_id = context.defining_identifier()
                if def_id:
                    self._define_symbol(def_id.getText(), TypeClassification.TypeName_, def_id.start, True)
                return
            if isinstance(context, AdaParser.Generic_instantiationContext):
                dpuns = context.defining_program_unit_name()
                if dpuns:
                    dpun = dpuns[0] if isinstance(dpuns, list) else dpuns
                    def_id = dpun.defining_identifier()
                    if def_id:
                        tc = TypeClassification.PackageName_
                        if context.PROCEDURE() or context.FUNCTION():
                            tc = TypeClassification.SubprogramName_
                        self._define_symbol(def_id.getText(), tc, def_id.start)
                dds = context.defining_designator()
                if dds:
                    dd = dds[0] if isinstance(dds, list) else dds
                    dpun = dd.defining_program_unit_name()
                    if dpun and dpun.defining_identifier():
                        self._define_symbol(dpun.defining_identifier().getText(), TypeClassification.SubprogramName_, dpun.defining_identifier().start)
                return
            if isinstance(context, AdaParser.Object_renaming_declarationContext):
                def_id = context.defining_identifier()
                if def_id:
                    self._define_symbol(def_id.getText(), TypeClassification.ObjectName_, def_id.start)
                return
            if isinstance(context, AdaParser.Exception_renaming_declarationContext):
                def_id = context.defining_identifier()
                if def_id:
                    self._define_symbol(def_id.getText(), TypeClassification.ExceptionName_, def_id.start)
                return
            if isinstance(context, AdaParser.Package_renaming_declarationContext):
                dpun = context.defining_program_unit_name()
                if dpun and dpun.defining_identifier():
                    self._define_symbol(dpun.defining_identifier().getText(), TypeClassification.PackageName_, dpun.defining_identifier().start)
                return
            if isinstance(context, AdaParser.Formal_complete_type_declarationContext):
                def_id = context.defining_identifier()
                if def_id:
                    self._define_symbol(def_id.getText(), TypeClassification.TypeName_, def_id.start)
                return
            if isinstance(context, AdaParser.Formal_incomplete_type_declarationContext):
                def_id = context.defining_identifier()
                if def_id:
                    self._define_symbol(def_id.getText(), TypeClassification.TypeName_, def_id.start)
                return
            if isinstance(context, AdaParser.Formal_object_declarationContext):
                def_id_list = context.defining_identifier_list()
                if def_id_list:
                    for def_id in def_id_list.defining_identifier():
                        self._define_symbol(def_id.getText(), TypeClassification.ObjectName_, def_id.start)
                return
            if isinstance(context, AdaParser.Formal_package_declarationContext):
                def_id = context.defining_identifier()
                if def_id:
                    self._define_symbol(def_id.getText(), TypeClassification.PackageName_, def_id.start)
                return
            if isinstance(context, AdaParser.Parameter_specificationContext):
                def_id_list = context.defining_identifier_list()
                if def_id_list:
                    for def_id in def_id_list.defining_identifier():
                        self._define_symbol(def_id.getText(), TypeClassification.ObjectName_, def_id.start)
                return
            if isinstance(context, AdaParser.Loop_parameter_specificationContext):
                def_id = context.defining_identifier()
                if def_id:
                    self._define_symbol(def_id.getText(), TypeClassification.ObjectName_, def_id.start)
                return
            if isinstance(context, AdaParser.Iterator_specificationContext):
                def_id = context.defining_identifier()
                if def_id:
                    self._define_symbol(def_id.getText(), TypeClassification.ObjectName_, def_id.start)
                return
            if isinstance(context, AdaParser.Enumeration_literal_specificationContext):
                def_id = context.defining_identifier()
                if def_id:
                    self._define_symbol(def_id.getText(), TypeClassification.EnumerationLiteral_, def_id.start)
                return
            if isinstance(context, AdaParser.Choice_parameter_specificationContext):
                def_id = context.defining_identifier()
                if def_id:
                    self._define_symbol(def_id.getText(), TypeClassification.ObjectName_, def_id.start)
                return
            if isinstance(context, AdaParser.Entry_index_specificationContext):
                def_id = context.defining_identifier()
                if def_id:
                    self._define_symbol(def_id.getText(), TypeClassification.ObjectName_, def_id.start)
                return
            if isinstance(context, AdaParser.Extended_return_object_declarationContext):
                def_id = context.defining_identifier()
                if def_id:
                    self._define_symbol(def_id.getText(), TypeClassification.ObjectName_, def_id.start)
                return
            if isinstance(context, AdaParser.Entry_bodyContext):
                def_id = context.defining_identifier()
                if def_id:
                    self._define_symbol(def_id.getText(), TypeClassification.SubprogramName_, def_id.start)
                return
            if isinstance(context, AdaParser.Task_bodyContext):
                def_id = context.defining_identifier()
                if def_id:
                    self._define_symbol(def_id.getText(), TypeClassification.ObjectName_, def_id.start)
                return
            if isinstance(context, AdaParser.Protected_bodyContext):
                def_id = context.defining_identifier()
                if def_id:
                    self._define_symbol(def_id.getText(), TypeClassification.ObjectName_, def_id.start)
                return
            if isinstance(context, AdaParser.Discriminant_specificationContext):
                def_id_list = context.defining_identifier_list()
                if def_id_list:
                    for def_id in def_id_list.defining_identifier():
                        self._define_symbol(def_id.getText(), TypeClassification.ObjectName_, def_id.start)
                return
            context = context.parentCtx

    def _define_subprogram_from_spec(self, spec):
        if "." in __name__:
            from .TypeClassification import TypeClassification
        else:
            from TypeClassification import TypeClassification
        if spec is None:
            return
        proc_spec = spec.procedure_specification()
        if proc_spec:
            dpun = proc_spec.defining_program_unit_name()
            if dpun and dpun.defining_identifier():
                self._define_symbol(dpun.defining_identifier().getText(), TypeClassification.SubprogramName_, dpun.defining_identifier().start)
            return
        func_spec = spec.function_specification()
        if func_spec:
            dd = func_spec.defining_designator()
            if dd:
                dpun = dd.defining_program_unit_name()
                if dpun and dpun.defining_identifier():
                    self._define_symbol(dpun.defining_identifier().getText(), TypeClassification.SubprogramName_, dpun.defining_identifier().start)

    def _define_symbol(self, name, classification, token, is_composite=False):
        if "." in __name__:
            from .Symbol import Symbol
        else:
            from Symbol import Symbol
        sym = Symbol()
        sym.name = name
        sym.classification = {classification}
        sym.is_composite = is_composite
        if token and hasattr(token, 'source') and token.source[0]:
            try:
                sym.defined_file = token.source[0].sourceName or ""
            except AttributeError:
                sym.defined_file = ""
        sym.defined_line = token.line if token else 0
        sym.defined_column = token.column if token else 0
        self._st.define(sym)

    def EnterScope(self):
        self._st.push_block_scope()

    def ExitScope(self):
        self._st.pop_block_scope()

    def PushExpectedType(self):
        self._expected_type_stack.append(None)

    def PopExpectedType(self):
        if self._expected_type_stack:
            self._expected_type_stack.pop()

    def OutputSymbolTable(self):
        if self._output_symbol_table:
            print(str(self._st), file=sys.stderr)

    def ImportWithClause(self):
        if "IsTypeName" in self._no_semantics and "IsAggregate" in self._no_semantics:
            return

        # Auto-detect current file from token stream
        if not self._current_file:
            stream = self._input
            if hasattr(stream, 'tokenSource') and stream.tokenSource:
                source_name = getattr(stream.tokenSource, 'sourceName', None)
                if source_name and source_name != "unknown" and os.path.isfile(source_name):
                    self._current_file = os.path.abspath(source_name)

        if "." in __name__:
            from .AdaParser import AdaParser
            from .AdaLexer import AdaLexer
            from .TypeClassification import TypeClassification
            from .Symbol import Symbol
        else:
            from AdaParser import AdaParser
            from AdaLexer import AdaLexer
            from TypeClassification import TypeClassification
            from Symbol import Symbol

        context = self._ctx
        names = None
        if isinstance(context, AdaParser.Nonlimited_with_clauseContext):
            names = context.name()
        elif isinstance(context, AdaParser.Limited_with_clauseContext):
            names = context.name()
        if not names:
            return

        for name_ctx in names:
            package_name = name_ctx.getText()
            if self._debug:
                print(f"ImportWithClause: processing 'with {package_name}'", file=sys.stderr)

            file_name = self._package_name_to_file_name(package_name)
            cache_key = package_name.lower()

            if cache_key in AdaParserBase._package_cache:
                if self._debug:
                    print(f"ImportWithClause: using cached symbols for {package_name}", file=sys.stderr)
                for sym in AdaParserBase._package_cache[cache_key]:
                    copy = Symbol()
                    copy.name = sym.name
                    copy.classification = set(sym.classification)
                    copy.is_composite = sym.is_composite
                    copy.defined_file = sym.defined_file
                    copy.defined_line = sym.defined_line
                    copy.defined_column = sym.defined_column
                    self._st.define(copy)
                continue

            ads_path = self._find_ads_file(file_name)
            if ads_path is None:
                if self._debug:
                    print(f"ImportWithClause: could not find {file_name}", file=sys.stderr)
                continue

            full_path = os.path.abspath(ads_path).lower()
            if full_path in AdaParserBase._parsing_in_progress:
                if self._debug:
                    print(f"ImportWithClause: skipping {file_name} (cycle detected)", file=sys.stderr)
                continue

            symbols = self._parse_ads_file(ads_path)
            if symbols is not None:
                AdaParserBase._package_cache[cache_key] = symbols
                for sym in symbols:
                    copy = Symbol()
                    copy.name = sym.name
                    copy.classification = set(sym.classification)
                    copy.is_composite = sym.is_composite
                    copy.defined_file = sym.defined_file
                    copy.defined_line = sym.defined_line
                    copy.defined_column = sym.defined_column
                    self._st.define(copy)
                    if self._debug:
                        print(f"ImportWithClause: imported symbol {sym.name} from {package_name}", file=sys.stderr)

    def _package_name_to_file_name(self, package_name):
        return package_name.lower().replace('.', '-') + ".ads"

    def _find_ads_file(self, file_name):
        if self._current_file:
            dir_path = os.path.dirname(self._current_file)
            if dir_path:
                candidate = os.path.join(dir_path, file_name)
                if os.path.isfile(candidate):
                    return candidate
        for search_path in self._search_paths:
            candidate = os.path.join(search_path, file_name)
            if os.path.isfile(candidate):
                return candidate
        return None

    def _parse_ads_file(self, ads_path):
        if "." in __name__:
            from .AdaParser import AdaParser
            from .AdaLexer import AdaLexer
        else:
            from AdaParser import AdaParser
            from AdaLexer import AdaLexer
        full_path = os.path.abspath(ads_path).lower()
        AdaParserBase._parsing_in_progress.add(full_path)
        try:
            if self._debug:
                print(f"ImportWithClause: parsing {ads_path}", file=sys.stderr)
            from antlr4 import CommonTokenStream, InputStream
            with open(ads_path, 'r') as f:
                input_stream = InputStream(f.read())
            lexer = AdaLexer(input_stream)
            lexer.removeErrorListeners()
            token_stream = CommonTokenStream(lexer)
            parser = AdaParser(token_stream)
            parser.removeErrorListeners()
            parser._current_file = os.path.abspath(ads_path)
            parser.compilation()
            return parser._st.get_exported_symbols()
        except Exception as ex:
            if self._debug:
                print(f"ImportWithClause: error parsing {ads_path}: {ex}", file=sys.stderr)
            return None
        finally:
            AdaParserBase._parsing_in_progress.discard(full_path)

    def ParsePragmas(self):
        if "." in __name__:
            from .AdaLexer import AdaLexer
            from .AdaParser import AdaParser
        else:
            from AdaLexer import AdaLexer
            from AdaParser import AdaParser
        stream = self._input
        stream.fill()
        all_tokens = stream.tokens
        PRAGMA_CHANNEL = 2
        current_pragma = None
        pragmas = []
        for token in all_tokens:
            if token.channel != PRAGMA_CHANNEL:
                continue
            if token.type == AdaLexer.PRAGMA:
                current_pragma = [token]
            elif current_pragma is not None:
                current_pragma.append(token)
                if token.type == AdaLexer.SEMI:
                    pragmas.append(current_pragma)
                    current_pragma = None
        for pragma_tokens in pragmas:
            default_channel_tokens = []
            for t in pragma_tokens:
                ct = CommonToken(source=(t.source[0], t.source[1]), type=t.type)
                ct.text = t.text
                ct.line = t.line
                ct.column = t.column
                ct.start = t.start
                ct.stop = t.stop
                ct.tokenIndex = t.tokenIndex
                ct.channel = Token.DEFAULT_CHANNEL
                default_channel_tokens.append(ct)
            eof = CommonToken(type=Token.EOF)
            eof.channel = Token.DEFAULT_CHANNEL
            default_channel_tokens.append(eof)
            token_source = CommonTokenStream(InputStream(""))
            token_source.tokens = default_channel_tokens
            token_source.index = 0
            token_source.fetchedEOF = True
            parser = AdaParser(token_source)
            parser.removeErrorListeners()
            for listener in self._listeners:
                parser.addErrorListener(listener)
            parser.pragmaRule()
