import re, sys

# Spec alphabetical order (parser rules only, from nt-bnf appendix)
# Non-spec helper rules placed near their related spec rule.
SPEC_ORDER = [
    "abbreviatedstep", "absolutepathexpr", "additiveexpr", "andexpr",
    "anyarraytype", "anyfunctiontype", "anyitemtype", "anymaptype", "anyrecordtype", "anyxnodetype",
    "argument", "argumentlist", "argumentplaceholder",
    "arrayconstructor", "arraytype",
    "arrowexpr", "arrowtarget",
    "attributename", "attributenodetype",
    "axis", "axisstep",
    "bracedaction",
    "castableexpr", "castexpr", "casttarget",
    "choiceitemtype", "commentnodetype", "comparisonexpr",
    "compattrconstructor", "compcommentconstructor", "compdocconstructor",
    "compelemconstructor", "compnsconstructor", "compnodename", "compnodencname",
    "comppiconstructor", "comptextconstructor", "computedconstructor",
    "constant", "contextvalueref",
    "curlyarrayconstructor",
    "defaultelementnamespacedecl",
    "documentnodetype",
    "dynamicnodetest",
    "elementname", "elementnodetype",
    "enclosedcontentexpr", "enclosedexpr",
    "enumerationtype",
    "eqname",
    "expr", "exprsingle",
    "fielddeclaration", "fieldname",
    "forbinding", "forclause", "forentrybinding", "forentrykeybinding", "forentryvaluebinding",
    "forexpr", "foritembinding", "forletreturn", "formemberbinding",
    "fullstep",
    "functionbody", "functioncall", "functionitemexpr", "functionsignature", "functiontype",
    "generalcomp",
    "gnodetype",
    "ifexpr",
    "inlinefunctionexpr",
    "instanceofexpr", "intersectexceptexpr",
    "itemtype",
    "jnodetype", "jrootselector",
    "keyspecifier", "keywordargument", "keywordarguments",
    "letarraybinding", "letbinding", "letclause", "letexpr",
    "letmapbinding", "letsequencebinding", "letvaluebinding",
    "literal", "lookup", "lookupwildcard",
    "mapconstructor", "mapconstructorentry",
    "mappingarrowtarget",
    "maptype", "markedncname",
    "multiplicativeexpr",
    "namedfunctionref",
    "namespacedecl", "namespacenodetype",
    "nametest", "nametestunion",
    "nodecomp", "nodeconstructor", "nodefollows", "nodeprecedes", "nodetest",
    "numericliteral",
    "occurrenceindicator",
    "orexpr", "otherwiseexpr",
    "paramlist", "parenthesizedexpr",
    "pathexpr", "pipelineexpr",
    "positionalargumentlist", "positionalarguments", "positionalvar",
    "postfixexpr",
    "predicate", "predicatelist",   # predicatelist: our helper, placed near predicate
    "primaryexpr",
    "processinginstructionnodetype",
    "qnameliteral",
    "quantifiedexpr", "quantifierbinding",
    "rangeexpr",
    "recordputexpr", "recordtype", "regularitemtype",
    "relativepathexpr", "restricteddynamiccall",
    "schemaattributenodetype", "schemaelementnodetype",
    "selector",
    "sequencearrowtarget", "sequencetype",
    "simplemapexpr", "simplenodetest",
    "squarearrayconstructor",
    "stepexpr",
    "stringconcatexpr", "stringtemplate",
    "textnodetype",
    "treatexpr",
    "typedarraytype", "typedeclaration",
    "typedfunctionparam", "typedfunctionparamlist", "typedfunctiontype",
    "typedmaptype", "typedrecordtype",
    "typename_", "simpletypename",   # simpletypename: our helper, placed after typename_
    "typetest",
    "unaryexpr", "unarylookup", "unbracedactions",
    "unionexpr", "unionnodetest",
    "uriliteral",
    "valuecomp", "valueexpr",
    "varnameandtype", "varref",
    "wildcard",
    "xnodetype",
    "xpath",
    "auxilary",   # test-only entry point, at end
]

# Case-insensitive lookup key: map lowered name -> original (grammar uses mixed case in some names)
# The grammar rule names are already lowercase in ANTLR4 convention, but some like
# compAttrconstructor have mixed case. We normalise to lowercase for matching.

with open("xpath/xpath4/XPath4Parser.g4") as f:
    text = f.read()

lines = text.split('\n')

RULE_LINE = re.compile(r'^([a-z][A-Za-z_0-9]*)\b')

def rule_name_of(line):
    m = RULE_LINE.match(line)
    if not m:
        return None
    name = m.group(1)
    if name in ('options', 'tokens', 'channels', 'parser', 'lexer', 'grammar'):
        return None
    return name

# Collect (line_index, original_name) for all rule definition lines
rule_name_indices = []
for i, line in enumerate(lines):
    name = rule_name_of(line)
    if name:
        rule_name_indices.append((i, name))

def find_block_start(idx):
    """Walk backwards from rule name line to include preceding // comment lines."""
    i = rule_name_indices[idx][0]
    start = i
    j = i - 1
    while j >= 0:
        stripped = lines[j].strip()
        if stripped.startswith('//'):
            start = j
            j -= 1
        elif stripped == '':
            break
        else:
            break
    return start

# Build blocks: list of (lowered_name, original_name, block_text)
blocks = []
for idx, (line_i, orig_name) in enumerate(rule_name_indices):
    block_start = find_block_start(idx)
    if idx + 1 < len(rule_name_indices):
        next_start = find_block_start(idx + 1)
        block_lines = lines[block_start:next_start]
    else:
        block_lines = lines[block_start:]
    # Strip trailing blank lines
    while block_lines and block_lines[-1].strip() == '':
        block_lines.pop()
    blocks.append((orig_name.lower(), orig_name, '\n'.join(block_lines)))

# Header: everything before first block
first_block_start = find_block_start(0)
header_lines = lines[:first_block_start]
while header_lines and header_lines[-1].strip() == '':
    header_lines.pop()
header = '\n'.join(header_lines)

# Build lookup: lowered_name -> block_text
block_map = {}
for lower, orig, text in blocks:
    block_map[lower] = text

# Warn about mismatches
spec_set = set(SPEC_ORDER)
ok = True
for lower, orig, _ in blocks:
    if lower not in spec_set:
        print(f"WARNING: grammar rule '{orig}' not in SPEC_ORDER", file=sys.stderr)
        ok = False
for name in SPEC_ORDER:
    if name not in block_map:
        print(f"WARNING: SPEC_ORDER '{name}' missing from grammar", file=sys.stderr)
        ok = False

# Build output
out_parts = [header]
for spec_name in SPEC_ORDER:
    if spec_name in block_map:
        out_parts.append('')
        out_parts.append(block_map[spec_name])

output = '\n'.join(out_parts) + '\n'

with open("xpath/xpath4/XPath4Parser.g4", "w") as f:
    f.write(output)

print(f"Done. {len(blocks)} rules reordered per spec alphabetical order.")
