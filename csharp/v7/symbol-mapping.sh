#!/usr/bin/env bash
# gen-symbol-mapping.sh — Print a Markdown table mapping every ANTLR4 grammar
# rule in CSharpParser.g4 / CSharpLexer.g4 to its counterpart in the
# ECMA-334 7th-edition (December 2023) specification EBNF.
#
# Usage:
#   bash gen-symbol-mapping.sh [PDF_PATH]
#
# PDF_PATH  Path to the ECMA-334 EBNF PDF (the annex-only extract used to
#           build this grammar).  Defaults to:
#             $HOME/Documents/ECMA-334_7th_edition_december_2023-ebnf.pdf
#
# Output    Two Markdown tables on stdout (CSharpParser rules, CSharpLexer
#           rules).  Pipe to a file or copy into README.md as needed.
#
# Requirements: pdftotext (poppler-utils), python3

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PDF="${1:-$HOME/Documents/ECMA-334_7th_edition_december_2023-ebnf.pdf}"
PARSER_G4="$SCRIPT_DIR/CSharpParser.g4"
LEXER_G4="$SCRIPT_DIR/CSharpLexer.g4"

err() { echo "error: $*" >&2; exit 1; }

[[ -f "$PDF"       ]] || err "PDF not found: $PDF"
[[ -f "$PARSER_G4" ]] || err "not found: $PARSER_G4"
[[ -f "$LEXER_G4"  ]] || err "not found: $LEXER_G4"
command -v pdftotext >/dev/null 2>&1 || err "pdftotext not in PATH (install poppler-utils)"
command -v python3   >/dev/null 2>&1 || err "python3 not in PATH"

EBNF_TMP=$(mktemp /tmp/ecma334-ebnf-XXXXXX.txt)
trap 'rm -f "$EBNF_TMP"' EXIT

pdftotext -enc UTF-8 "$PDF" "$EBNF_TMP"

PYTHONIOENCODING=utf-8 python3 - "$PARSER_G4" "$LEXER_G4" "$EBNF_TMP" <<'PYEOF'
import re, sys

parser_g4_path, lexer_g4_path, ebnf_txt_path = sys.argv[1], sys.argv[2], sys.argv[3]

# ──────────────────────────────────────────────────────────────────────────────
# 1.  Parse spec EBNF: build  normalised_name -> (canonical_name, section)
#
#     The pdftotext output contains blocks like:
#       // Source: §6.3.1 General
#       input : input_section? ;
#     or, when the section introduces a single rule:
#       // Source: §6.3.2 Line terminators New_Line
#       : New_Line_Character | ... ;
#
#     We track the current section from each "// Source:" comment and collect
#     every rule name that is defined (or hinted) under it.
# ──────────────────────────────────────────────────────────────────────────────

def _norm(name):
    """Normalise an identifier for fuzzy lookup.

    Strips a trailing underscore, converts camelCase to snake_case, then
    lowercases everything.  Examples:
        type_              -> type
        declarationStatement -> declaration_statement
        labeled_Statement  -> labeled_statement
        Integer_Literal    -> integer_literal
    """
    n = re.sub(r'([a-z])([A-Z])', r'\1_\2', name).lower().rstrip('_')
    return n

SOURCE_PAT   = re.compile(r'// Source: .?([\d]+\.[\d.]+)(.*)')
RULE_DEF_PAT = re.compile(r'^(?:fragment\s+)?([A-Za-z_][A-Za-z0-9_]*)\s*:')
RULE_BARE_PAT= re.compile(r'^(?:fragment\s+)?([A-Za-z_][A-Za-z0-9_]*)\s*$')

# Common English words that appear in section titles and must not be mistaken
# for rule names embedded in "// Source:" comments.
SKIP_WORDS = {
    'A', 'Grammar', 'General', 'This', 'annex', 'contains', 'the', 'optional',
    'Productions', 'appear', 'here', 'in', 'same', 'order', 'which', 'they',
    'specification', 'ECMA', 'EOF', 'Lexical', 'Syntactic', 'unsafe', 'code',
    'is', 'informative', 'are', 'including', 'for', 'ones', 'found', 'grammar',
    'productions', 'Source',
}

spec = {}           # normalised_name -> (canonical_name, section)
cur_section = None

with open(ebnf_txt_path, encoding='utf-8', errors='replace') as f:
    lines = f.read().splitlines()

i = 0
while i < len(lines):
    line = lines[i].strip()

    m = SOURCE_PAT.search(line)
    if m:
        cur_section = f"§{m.group(1)}"
        # Sometimes the rule name is the last token of the Source comment, e.g.:
        #   // Source: §6.3.2 Line terminators New_Line
        rest_tokens = m.group(2).split()
        if rest_tokens:
            last = rest_tokens[-1]
            if re.match(r'^[A-Za-z_][A-Za-z0-9_]*$', last) and last not in SKIP_WORDS:
                k = _norm(last)
                if k not in spec:
                    spec[k] = (last, cur_section)
        i += 1
        continue

    if cur_section is None or not line or re.match(r'^\d+$', line) or line.startswith('//'):
        i += 1
        continue

    m2 = RULE_DEF_PAT.match(line)
    if m2:
        rn = m2.group(1)
        if rn not in SKIP_WORDS and len(rn) > 1:
            k = _norm(rn)
            if k not in spec:
                spec[k] = (rn, cur_section)
        i += 1
        continue

    # Rule name on its own line; colon on a nearby following line.
    # We look up to 10 lines ahead, skipping blank lines, page numbers, and
    # common PDF page-header text (e.g. "A Grammar", "ECMA-334") that
    # pdftotext inserts between sections.
    m3 = RULE_BARE_PAT.match(line)
    if m3:
        rn = m3.group(1)
        if rn not in SKIP_WORDS and len(rn) > 1:
            j = i + 1
            found_colon = False
            while j < len(lines) and j < i + 10:
                nxt = lines[j].strip()
                # Skip blank lines, page numbers, and PDF page headers
                if (not nxt
                        or re.match(r'^\d+$', nxt)
                        or nxt in ('A Grammar', 'ECMA-334', 'A Grammar;')):
                    j += 1
                    continue
                if nxt.startswith(':'):
                    found_colon = True
                break
            if found_colon:
                k = _norm(rn)
                if k not in spec:
                    spec[k] = (rn, cur_section)
    i += 1

# ──────────────────────────────────────────────────────────────────────────────
# 2.  Explicit override table
#
#     Maps ANTLR4 rule name -> (spec_symbol, section).
#     Use (None, None) for rules with no spec equivalent (ANTLR-specific).
#     The spec_symbol may contain ' / ' to list multiple alternatives.
# ──────────────────────────────────────────────────────────────────────────────

_AS = (None, None)   # shorthand for ANTLR-specific

OVERRIDES = {
    # ── Types ──────────────────────────────────────────────────────────────
    'type_':                                ('type',                                            '§8.1'),
    'base_type':                            _AS,
    'tuple_element':                        ('tuple_type_element',                              '§8.3.1'),

    # ── Expressions ────────────────────────────────────────────────────────
    'primary_expression_start':             ('primary_no_array_creation_expression',            '§12.8.1'),
    'throwable_expression':                 _AS,
    'bracket_expression':                   ('element_access',                                  '§12.8.11.1'),
    'indexer_argument':                     ('argument',                                        '§12.6.2.1'),
    'string_literal':                       ('String_Literal',                                  '§6.4.5.6'),
    'interpolated_regular_string':          ('interpolated_regular_string_expression',          '§12.8.3'),
    'interpolated_verbatium_string':        ('interpolated_verbatim_string_expression',         '§12.8.3'),
    'interpolated_regular_string_part':     ('Interpolated_Regular_String_Element',             '§12.8.3'),
    'interpolated_verbatium_string_part':   ('Interpolated_Verbatim_String_Element',            '§12.8.3'),
    'interpolated_string_expression':       ('regular_interpolation / verbatim_interpolation',  '§12.8.3'),
    'combined_join_clause':                 ('join_clause / join_into_clause',                  '§12.20.1'),
    'overloadable_operator':                ('overloadable_unary_operator / overloadable_binary_operator', '§15.10.1'),
    'attribute_argument':                   ('positional_argument / named_argument',            '§22.3'),
    'method_invocation':                    ('invocation_expression',                           '§12.8.9.1'),
    'right_arrow':                          _AS,

    # ── Statements ─────────────────────────────────────────────────────────
    'declarationStatement':                 ('declaration_statement',                           '§13.6.1'),
    'labeled_Statement':                    ('labeled_statement',                               '§13.5'),
    'embedded_statement':                   ('embedded_statement',                              '§13.1'),
    'simple_embedded_statement':            ('*(covers §13.4, §13.7–§13.15)*',                 ''),
    'if_body':                              _AS,
    'simple_designation':                   ('declaration_pattern / var_pattern',               '§11.2.2 / §11.2.4'),
    'local_variable_declarator':            ('local_variable_declarator',                       '§13.6.2.2'),
    'local_function_modifiers':             ('local_function_modifier',                         '§13.6.4'),

    # ── Query expressions ───────────────────────────────────────────────────
    'query_body':                           ('query_body',                                      '§12.20.1'),

    # ── Namespaces ──────────────────────────────────────────────────────────
    'extern_alias_directives':              _AS,
    'using_directives':                     _AS,
    'namespace_member_declarations':        _AS,

    # ── Classes / members ──────────────────────────────────────────────────
    'class_member_declarations':            _AS,
    'all_member_modifiers':                 _AS,
    'all_member_modifier':                  _AS,
    'common_member_declaration':            _AS,
    'typed_member_declaration':             _AS,
    'body':                                 _AS,
    'method_member_name':                   _AS,
    'arg_declaration':                      ('fixed_parameter',                                 '§15.6.2.1'),
    'class_definition':                     ('class_declaration',                               '§15.2.1'),
    'struct_definition':                    ('struct_declaration',                              '§16.2.1'),
    'interface_definition':                 ('interface_declaration',                           '§18.2.1'),
    'enum_definition':                      ('enum_declaration',                                '§19.2'),
    'delegate_definition':                  ('delegate_declaration',                            '§20.2'),
    'destructor_definition':                ('finalizer_declaration',                           '§15.13'),
    'variant_type_parameter':               ('variant_type_parameters',                        '§18.2.3.1'),

    # ── Lexer: comment / whitespace tokens ─────────────────────────────────
    'BYTE_ORDER_MARK':                      _AS,
    'SINGLE_LINE_DOC_COMMENT':              ('Single_Line_Comment',                             '§6.3.3'),
    'EMPTY_DELIMITED_DOC_COMMENT':          ('Delimited_Comment',                               '§6.3.3'),
    'DELIMITED_DOC_COMMENT':                ('Delimited_Comment',                               '§6.3.3'),
    'SINGLE_LINE_COMMENT':                  ('Single_Line_Comment',                             '§6.3.3'),
    'DELIMITED_COMMENT':                    ('Delimited_Comment',                               '§6.3.3'),
    'WHITESPACES':                          ('Whitespace',                                      '§6.3.4'),
    'SHARP':                                _AS,

    # ── Lexer: identifier & literals ───────────────────────────────────────
    'IDENTIFIER':                           ('Simple_Identifier',                               '§6.4.3'),
    'LITERAL_ACCESS':                       _AS,
    'INTEGER_LITERAL':                      ('Decimal_Integer_Literal',                         '§6.4.5.3'),
    'HEX_INTEGER_LITERAL':                  ('Hexadecimal_Integer_Literal',                     '§6.4.5.3'),
    'BIN_INTEGER_LITERAL':                  ('Binary_Integer_Literal',                          '§6.4.5.3'),
    'REAL_LITERAL':                         ('Real_Literal',                                    '§6.4.5.4'),
    'CHARACTER_LITERAL':                    ('Character_Literal',                               '§6.4.5.5'),
    'REGULAR_STRING':                       ('Regular_String_Literal',                          '§6.4.5.6'),
    'VERBATIUM_STRING':                     ('Verbatim_String_Literal',                         '§6.4.5.6'),

    # ── Lexer: interpolated string tokens ──────────────────────────────────
    'INTERPOLATED_REGULAR_STRING_START':    ('Interpolated_Regular_String_Start',               '§12.8.3'),
    'INTERPOLATED_VERBATIUM_STRING_START':  ('Interpolated_Verbatim_String_Start',              '§12.8.3'),
    'DOUBLE_CURLY_INSIDE':                  ('Open_Brace_Escape_Sequence',                      '§12.8.3'),
    'REGULAR_CHAR_INSIDE':                  ('Interpolated_Regular_String_Element',             '§12.8.3'),
    'VERBATIUM_DOUBLE_QUOTE_INSIDE':        ('Interpolated_Verbatim_String_Character',          '§12.8.3'),
    'DOUBLE_QUOTE_INSIDE':                  ('Interpolated_Regular_String_End',                 '§12.8.3'),
    'REGULAR_STRING_INSIDE':                ('Interpolated_Regular_String_Element',             '§12.8.3'),
    'VERBATIUM_INSIDE_STRING':              ('Interpolated_Verbatim_String_Element',            '§12.8.3'),
    'DOUBLE_CURLY_CLOSE_INSIDE':            ('Close_Brace_Escape_Sequence',                     '§12.8.3'),
    'FORMAT_STRING':                        ('Regular_Interpolation_Format / Verbatim_Interpolation_Format', '§12.8.3'),
    'OPEN_BRACE_INSIDE':                    _AS,
    'CLOSE_BRACE_INSIDE':                   _AS,

    # ── Lexer: directive-mode tokens ───────────────────────────────────────
    'DIGITS':                               ('Decimal_Integer_Literal',                         '§6.4.5.3'),
    'DIRECTIVE_WHITESPACES':                ('Whitespace',                                      '§6.3.4'),
    'DIRECTIVE_TRUE':                       ('keyword',                                         '§6.4.4'),
    'DIRECTIVE_FALSE':                      ('keyword',                                         '§6.4.4'),
    'DIRECTIVE_DEFAULT':                    ('keyword',                                         '§6.4.4'),
    'DIRECTIVE_IF':                         ('PP_If_Section',                                   '§6.5.5'),
    'DIRECTIVE_ELSE':                       ('PP_Else_Section',                                 '§6.5.5'),
    'DEFINE':                               ('PP_Declaration',                                  '§6.5.4'),
    'UNDEF':                                ('PP_Declaration',                                  '§6.5.4'),
    'ELIF':                                 ('PP_Elif_Section',                                 '§6.5.5'),
    'ENDIF':                                ('PP_Endif',                                        '§6.5.5'),
    'LINE':                                 ('PP_Line',                                         '§6.5.8'),
    'ERROR':                                ('PP_Diagnostic',                                   '§6.5.6'),
    'WARNING':                              ('PP_Diagnostic',                                   '§6.5.6'),
    'REGION':                               ('PP_Start_Region',                                 '§6.5.7'),
    'ENDREGION':                            ('PP_End_Region',                                   '§6.5.7'),
    'PRAGMA':                               ('PP_Pragma',                                       '§6.5.9'),
    'NULLABLE':                             _AS,
    'DIRECTIVE_HIDDEN':                     ('PP_Line_Indicator',                               '§6.5.8'),
    'DIRECTIVE_OPEN_PARENS':                ('operator_or_punctuator',                          '§6.4.6'),
    'DIRECTIVE_CLOSE_PARENS':               ('operator_or_punctuator',                          '§6.4.6'),
    'DIRECTIVE_BANG':                       ('operator_or_punctuator',                          '§6.4.6'),
    'DIRECTIVE_OP_EQ':                      ('operator_or_punctuator',                          '§6.4.6'),
    'DIRECTIVE_OP_NE':                      ('operator_or_punctuator',                          '§6.4.6'),
    'DIRECTIVE_OP_AND':                     ('PP_And_Expression',                               '§6.5.3'),
    'DIRECTIVE_OP_OR':                      ('PP_Or_Expression',                                '§6.5.3'),
    'DIRECTIVE_STRING':                     ('String_Literal',                                  '§6.4.5.6'),
    'CONDITIONAL_SYMBOL':                   ('PP_Primary_Expression',                           '§6.5.3'),
    'DIRECTIVE_SINGLE_LINE_COMMENT':        ('Single_Line_Comment',                             '§6.3.3'),
    'DIRECTIVE_NEW_LINE':                   ('PP_New_Line',                                     '§6.5.1'),
    'TEXT':                                 ('PP_Message / PP_Pragma_Text',                     '§6.5.6 / §6.5.9'),
    'TEXT_NEW_LINE':                        ('PP_New_Line',                                     '§6.5.1'),
}

# Keyword tokens: all map to keyword / contextual_keyword §6.4.4 and are
# collapsed to a single representative row in the output table.
KEYWORD_TOKENS = {
    'ABSTRACT','ADD','ALIAS','ARGLIST','AS','ASCENDING','ASYNC','AWAIT','BASE',
    'BOOL','BREAK','BY','BYTE','CASE','CATCH','CHAR','CHECKED','CLASS','CONST',
    'CONTINUE','DECIMAL','DEFAULT','DELEGATE','DESCENDING','DO','DOUBLE','DYNAMIC',
    'ELSE','ENUM','EQUALS','EVENT','EXPLICIT','EXTERN','FALSE','FINALLY','FIXED',
    'FLOAT','FOR','FOREACH','FROM','GET','GOTO','GROUP','IF','IMPLICIT','IN',
    'INT','INTERFACE','INTERNAL','INTO','IS','JOIN','LET','LOCK','LONG','NAMEOF',
    'NAMESPACE','NEW','NULL_','OBJECT','ON','OPERATOR','ORDERBY','OUT','OVERRIDE',
    'PARAMS','PARTIAL','PRIVATE','PROTECTED','PUBLIC','READONLY','REF','REMOVE',
    'RETURN','SBYTE','SEALED','SELECT','SET','SHORT','SIZEOF','STACKALLOC','STATIC',
    'STRING','STRUCT','SWITCH','THIS','THROW','TRUE','TRY','TYPEOF','UINT','ULONG',
    'UNCHECKED','UNMANAGED','UNSAFE','USHORT','USING','VAR','VIRTUAL','VOID',
    'VOLATILE','WHEN','WHERE','WHILE','YIELD',
}

# Operator / punctuator tokens: all map to operator_or_punctuator §6.4.6 and
# are collapsed to a single representative row.
OPERATOR_TOKENS = {
    'OPEN_BRACE','CLOSE_BRACE','OPEN_BRACKET','CLOSE_BRACKET','OPEN_PARENS',
    'CLOSE_PARENS','DOT','COMMA','COLON','SEMICOLON','PLUS','MINUS','STAR','DIV',
    'PERCENT','AMP','BITWISE_OR','CARET','BANG','TILDE','ASSIGNMENT','LT','GT',
    'INTERR','DOUBLE_COLON','OP_COALESCING','OP_INC','OP_DEC','OP_AND','OP_OR',
    'OP_PTR','OP_EQ','OP_NE','OP_LE','OP_GE','OP_ADD_ASSIGNMENT','OP_SUB_ASSIGNMENT',
    'OP_MULT_ASSIGNMENT','OP_DIV_ASSIGNMENT','OP_MOD_ASSIGNMENT','OP_AND_ASSIGNMENT',
    'OP_OR_ASSIGNMENT','OP_XOR_ASSIGNMENT','OP_LEFT_SHIFT','OP_LEFT_SHIFT_ASSIGNMENT',
    'OP_COALESCING_ASSIGNMENT','OP_RANGE',
}

# ──────────────────────────────────────────────────────────────────────────────
# 3.  Extract rule names from .g4 files (preserving document order)
# ──────────────────────────────────────────────────────────────────────────────

def extract_parser_rules(path):
    """Return ordered list of parser rule names from CSharpParser.g4.

    Parser rules in ANTLR4 start at column 0 with a lowercase identifier
    on its own line; their body (starting with ':') is on the next line(s).
    """
    rules = []
    with open(path, encoding='utf-8') as f:
        for line in f:
            # Strip trailing newline but keep leading spaces to check column 0.
            raw = line.rstrip('\n')
            stripped = raw.strip()
            if (not stripped
                    or stripped.startswith('//')
                    or stripped.startswith('$')
                    or stripped.startswith('parser grammar')
                    or stripped.startswith('options')
                    or stripped in ('{', '}')):
                continue
            m = re.match(r'^([a-z][A-Za-z0-9_]*)\s*$', raw)
            if m:
                rules.append(m.group(1))
    return rules

def extract_lexer_rules(path):
    """Return ordered list of (name, is_fragment) for all lexer rules.

    Lexer rules have their name and ':' on the same line.  Fragment rules
    are prefixed with 'fragment'.  Mode-switch lines ('mode NAME;') are
    detected and skipped.
    """
    rules = []
    with open(path, encoding='utf-8') as f:
        for line in f:
            raw = line.rstrip('\n')
            stripped = raw.strip()
            # Skip mode-switch statements, blanks, comments, directives
            if (not stripped
                    or stripped.startswith('//')
                    or stripped.startswith('$')
                    or re.match(r'^mode\s+\w+\s*;', stripped)
                    or stripped.startswith('channels')
                    or stripped in ('{', '}')):
                continue
            m = re.match(r'^(fragment\s+)?([A-Z_][A-Za-z0-9_]*)\s*:', raw)
            if m:
                rules.append((m.group(2), bool(m.group(1))))
    return rules

parser_rules  = extract_parser_rules(parser_g4_path)
lexer_entries = extract_lexer_rules(lexer_g4_path)

# ──────────────────────────────────────────────────────────────────────────────
# 4.  Resolve each grammar rule to (spec_symbol, section)
# ──────────────────────────────────────────────────────────────────────────────

def resolve(name):
    """Return (spec_symbol, section) for an ANTLR4 rule name.

    Lookup order:
      1. Explicit OVERRIDES table (handles renames, merges, ANTLR-specifics).
      2. Normalised fuzzy match against the spec rule dictionary.
      3. Fall back to ANTLR-specific if nothing matches.
    """
    if name in OVERRIDES:
        return OVERRIDES[name]
    k = _norm(name)
    if k in spec:
        return spec[k]
    return _AS

# ──────────────────────────────────────────────────────────────────────────────
# 5.  Emit Markdown tables
# ──────────────────────────────────────────────────────────────────────────────

def fmt_sym(sym):
    """Render a spec symbol (or None) as Markdown cell content."""
    if sym is None:
        return '*(ANTLR-specific)*'
    if sym.startswith('*('):       # pre-formatted note, e.g. "*(covers ...)*"
        return sym
    if ' / ' in sym:               # multiple alternatives
        return ' / '.join(f'`{p}`' for p in sym.split(' / '))
    return f'`{sym}`'

def emit_row(antlr_name, sym, sec):
    print(f'| `{antlr_name}` | {fmt_sym(sym)} | {sec or ""} |')

# ── CSharpParser table ────────────────────────────────────────────────────────
print('### CSharpParser rules\n')
print('| ANTLR4 Rule | Spec Symbol | Section |')
print('|---|---|---|')
for name in parser_rules:
    sym, sec = resolve(name)
    emit_row(name, sym, sec)

# ── CSharpLexer table ─────────────────────────────────────────────────────────
print()
print('### CSharpLexer rules\n')
print('| ANTLR4 Rule | Spec Symbol | Section |')
print('|---|---|---|')

kw_emitted = False
op_emitted = False

for name, is_frag in lexer_entries:
    if is_frag:
        continue    # fragment rules are implementation detail; no spec symbol

    if name in KEYWORD_TOKENS:
        if not kw_emitted:
            print('| `ABSTRACT` … `YIELD` *(keyword tokens)* '
                  '| `keyword` / `contextual_keyword` | §6.4.4 |')
            kw_emitted = True
        continue

    if name in OPERATOR_TOKENS:
        if not op_emitted:
            print('| `OPEN_BRACE` … `OP_RANGE` *(operator / punctuator tokens)* '
                  '| `operator_or_punctuator` | §6.4.6 |')
            op_emitted = True
        continue

    sym, sec = resolve(name)
    emit_row(name, sym, sec)
PYEOF
