from antlr4 import *
from antlr4.Token import CommonToken
import sys
from typing import TextIO


class AdaParserBase(Parser):

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
