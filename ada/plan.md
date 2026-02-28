# Plan: Add Ada 2022 Grammar

## Context

The grammars-v4 repository already has an Ada 2012 grammar at
`/c/msys64/home/Kenne/issues/g4-4761/ada/ada2012/`. The goal is to create a
new `ada2022/` directory at `/c/msys64/home/Kenne/issues/g4-4757/ada/ada2022/`
based on the Ada 2012 grammar with all grammar changes required by the
[Ada 2022 Language Reference Manual](http://www.ada-auth.org/standards/ada22.html).

The Ada 2022 EBNF is at `http://www.ada-auth.org/standards/22rm/html/RM-P-1.html`.
The Ada 2012 EBNF is at `http://www.ada-auth.org/standards/rm12_w_tc1/html/RM-P.html`.

Diffing the two EBNF pages reveals:
- **45 new grammar rules** in Ada 2022
- **29 modified existing rules**
- **1 new reserved word**: `parallel`
- **3 new lexical tokens**: `[`, `]`, `@`

## Source Files (ada2012 to copy from)

`/c/msys64/home/Kenne/issues/g4-4761/ada/ada2012/`
- `AdaLexer.g4`
- `AdaParser.g4`
- `desc.xml`
- `readme.md`
- `CSharp/` — AdaLexerBase.cs, AdaParserBase.cs, Symbol.cs, SymbolTable.cs, TypeClassification.cs
- `Java/` — same set in Java
- `Python3/` — same set in Python3
- `Go/` — same set in Go

**Note:** The `examples/` directory is NOT copied. Instead, `desc.xml` will
reference the ada2012 `examples/` via a relative path, and a new
`ada2022/examples/` directory will be created only for Ada 2022-specific tests.
- `Antlr4ng/` — same set in TypeScript
- `JavaScript/`, `TypeScript/`, `Dart/`, `Cpp/` — same pattern
- `examples/` — pkg1.adb, pkg1.ads, pragma_test.ads, semtest/, acats/

## Implementation Steps

---

### Step 1: Copy ada2012 → ada2022 (without examples)

Copy only the grammar, runtime support files, and build infrastructure —
**not** the `examples/` directory:

```bash
SRC=/c/msys64/home/Kenne/issues/g4-4761/ada/ada2012
DST=/c/msys64/home/Kenne/issues/g4-4757/ada/ada2022
mkdir -p $DST
cp $SRC/AdaLexer.g4 $SRC/AdaParser.g4 $SRC/desc.xml $SRC/readme.md \
   $SRC/pom.xml $SRC/gr.sh $SRC/te.sh $DST/
for d in CSharp Java Python3 Go Antlr4ng JavaScript TypeScript Dart Cpp; do
    cp -r $SRC/$d $DST/$d
done
mkdir $DST/examples
```

Then delete the downloaded scratch files (leave `plan.md` in place):
```bash
rm /c/msys64/home/Kenne/issues/g4-4757/ada/ada2022_ebnf.html \
   /c/msys64/home/Kenne/issues/g4-4757/ada/ada2012_ebnf.html
```

---

### Step 2: Update File Headers and Metadata

**`AdaParser.g4`** — change header comment from "Ada 2012 grammar" to "Ada 2022 grammar".

**`AdaLexer.g4`** — change header comment from "Ada 2012 grammar" to "Ada 2022 grammar".

**`readme.md`** — rewrite to describe Ada 2022, link to Ada 2022 RM, update
reference links (replace Ada 2012 RM URLs with Ada 2022 RM URLs).

**`desc.xml`** — update test inputs to add `examples/ada2022/*.adb` (see Step 6).
Keep the existing ACATS tests (they are Ada 2012 compliant and Ada 2022 is a
superset).

---

### Step 3: Lexer Changes — `AdaLexer.g4`

**Add new reserved word** (alphabetically in the keywords section):

```antlr
PARALLEL : 'parallel';
```

**Add new operator/punctuation tokens** (in the symbols section):

```antlr
LSB    : '[';
RSB    : ']';
AT_SIGN : '@';
```

**PRAGMA_MODE section** — add corresponding pragma-mode tokens for `[`, `]`, `@`
so that pragmas containing these tokens (unusual but possible) are tokenized:

```antlr
PRAGMA_LSB     : '['  -> channel(PRAGMA_CHANNEL), type(LSB);
PRAGMA_RSB     : ']'  -> channel(PRAGMA_CHANNEL), type(RSB);
PRAGMA_AT_SIGN : '@'  -> channel(PRAGMA_CHANNEL), type(AT_SIGN);
```

---

### Step 4: Parser Changes — `AdaParser.g4`

#### 4a. Modified existing rules

**(RM 4.3.3) `array_aggregate`** — add `null_array_aggregate`:
```antlr
array_aggregate
    : positional_array_aggregate
    | null_array_aggregate
    | named_array_aggregate
    ;
```

**(RM 4.3.3) `array_component_association`** — add `iterated_component_association`:
```antlr
array_component_association
    : discrete_choice_list ARROW expression
    | discrete_choice_list ARROW BOX
    | iterated_component_association
    ;
```

**(RM 4.3.4 / 4.3.5) `aggregate`** — add `delta_aggregate` and `container_aggregate`:
```antlr
aggregate
    : record_aggregate
    | extension_aggregate
    | array_aggregate
    | delta_aggregate
    | container_aggregate
    ;
```

**(RM 4.3.3) `positional_array_aggregate`** — add bracket forms:
```antlr
positional_array_aggregate
    : '(' expression ',' expression (',' expression)* ')'
    | '(' expression (',' expression)* ',' OTHERS ARROW expression ')'
    | '(' expression (',' expression)* ',' OTHERS ARROW BOX ')'
    | LSB expression (',' expression)* (COMMA OTHERS ARROW expression)? RSB
    | LSB expression (',' expression)* COMMA OTHERS ARROW BOX RSB
    ;
```

**(RM 4.3.3) `named_array_aggregate`** — add bracket form:
```antlr
named_array_aggregate
    : '(' array_component_association_list ')'
    | LSB array_component_association_list RSB
    ;
```

**(RM 4.5) `primary`** — add `declare_expression`:
```antlr
primary
    : numeric_literal
    | NULL_
    | string_literal
    | {this.IsAggregate()}? aggregate
    | name
    | allocator
    | '(' expression ')'
    | '(' conditional_expression ')'
    | '(' qualified_expression ')'
    | '(' declare_expression ')'
    ;
```

**(RM 4.1) `name`** — add `target_name`:
```antlr
name
    : direct_name
    | name DOT ALL
    | name '(' expression (',' expression)* ')'
    | name '(' discrete_range ')'
    | name DOT selector_name
    | name SQ attribute_designator
    | {this.IsTypeName()}? type_conversion
    | name actual_parameter_part
    | character_literal
    | {this.IsTypeName()}? qualified_expression
    | name actual_parameter_part
    | target_name
    ;
```

**(RM 4.1.4) `attribute_reference` / `attribute_designator`** — The
`reduction_attribute_reference` introduces a new form `value_sequence'designator`.
Since `value_sequence` starts with `[`, it is unambiguous in the `name` rule:
add it as a new alternative there:
```antlr
name
    ...
    | reduction_attribute_reference
    ;
```
(The `reduction_attribute_reference` starts with `[`, making it distinct from
all other `name` alternatives which start with an identifier, `'(`, or `@`.)

**(RM 5.1) `compound_statement`** — add `parallel_block_statement`:
```antlr
compound_statement
    : if_statement
    | case_statement
    | loop_statement
    | block_statement
    | extended_return_statement
    | parallel_block_statement
    | accept_statement
    | select_statement
    ;
```

**(RM 5.5) `iteration_scheme`** — add parallel loops and procedural_iterator:
```antlr
iteration_scheme
    : WHILE condition
    | FOR loop_parameter_specification
    | FOR iterator_specification
    | PARALLEL ('(' chunk_specification ')')? aspect_specification? FOR loop_parameter_specification
    | PARALLEL ('(' chunk_specification ')')? aspect_specification? FOR iterator_specification
    | PARALLEL aspect_specification? FOR procedural_iterator
    | FOR procedural_iterator
    ;
```

**(RM 5.5.2) `iterator_specification`** — use `loop_parameter_subtype_indication`,
add `iterator_filter`:
```antlr
iterator_specification
    : defining_identifier (COLON loop_parameter_subtype_indication)? IN REVERSE? name iterator_filter? {this.EnterDeclaration();}
    | defining_identifier (COLON loop_parameter_subtype_indication)? OF REVERSE? name iterator_filter? {this.EnterDeclaration();}
    ;
```

**(RM 5.5.2) `loop_parameter_specification`** — add `iterator_filter`:
```antlr
loop_parameter_specification
    : defining_identifier IN REVERSE? discrete_subtype_definition iterator_filter? {this.EnterDeclaration();}
    ;
```

**(RM 3.7) `discriminant_specification`** — add optional `aspect_specification`:
```antlr
discriminant_specification
    : defining_identifier_list COLON null_exclusion? subtype_mark (ASSIGN expression)? aspect_specification? {this.EnterDeclaration();}
    | defining_identifier_list COLON access_definition (ASSIGN expression)? aspect_specification? {this.EnterDeclaration();}
    ;
```

**(RM 9.5.2) `entry_body`** — add optional `aspect_specification`:
```antlr
entry_body
    : ENTRY defining_identifier entry_body_formal_part aspect_specification? entry_barrier IS
      {this.EnterDeclaration(); this.EnterScope();} declarative_part BEGIN handled_sequence_of_statements
      END entry_identifier? SEMI {this.ExitScope();}
    ;
```

**(RM 9.5.2) `entry_index_specification`** — add optional `aspect_specification`:
```antlr
entry_index_specification
    : FOR defining_identifier IN discrete_subtype_definition aspect_specification? {this.EnterDeclaration();}
    ;
```

**(RM 6.5) `extended_return_object_declaration`** — add optional `aspect_specification`:
```antlr
extended_return_object_declaration
    : defining_identifier COLON ALIASED? CONSTANT? return_subtype_indication (ASSIGN expression)? aspect_specification? {this.EnterDeclaration();}
    ;
```

**(RM 6.4) `parameter_specification`** — add optional `aspect_specification`:
```antlr
parameter_specification
    : defining_identifier_list COLON ALIASED? mode_ null_exclusion? subtype_mark (ASSIGN default_expression)? aspect_specification? {this.EnterDeclaration();}
    | defining_identifier_list COLON access_definition (ASSIGN default_expression)? aspect_specification? {this.EnterDeclaration();}
    ;
```

**(RM 12.5) `formal_complete_type_declaration`** — add `[OR USE default_subtype_mark]`:
```antlr
formal_complete_type_declaration
    : TYPE defining_identifier discriminant_part? IS formal_type_definition
      (OR USE subtype_mark)? aspect_specification? SEMI {this.EnterDeclaration();}
    ;
```

**(RM 12.5) `formal_incomplete_type_declaration`** — add `[OR USE default_subtype_mark]`:
```antlr
formal_incomplete_type_declaration
    : TYPE defining_identifier discriminant_part? (IS TAGGED)? (OR USE subtype_mark)? SEMI {this.EnterDeclaration();}
    ;
```

**(RM 8.5.1) `object_renaming_declaration`** — subtype_mark made optional:
```antlr
object_renaming_declaration
    : defining_identifier (COLON null_exclusion? subtype_mark)? RENAMES name aspect_specification? SEMI {this.EnterDeclaration();}
    | defining_identifier COLON access_definition RENAMES name aspect_specification? SEMI {this.EnterDeclaration();}
    ;
```

**(RM 3.8) `record_definition`** — add optional identifier after `end record`:
```antlr
record_definition
    : RECORD {this.EnterScope();} component_list END RECORD identifier? {this.ExitScope();}
    | NULL_ RECORD
    ;
```

**(RM 13.5.1) `record_representation_clause`** — add optional `local_name` after `end record`:
```antlr
record_representation_clause
    : FOR local_name USE RECORD mod_clause? component_clause* END RECORD local_name? SEMI
    ;
```

**(RM 13.1.1) `aspect_definition`** — add `aggregate | global_aspect_definition`:
```antlr
aspect_definition
    : name
    | expression
    | identifier
    | aggregate
    | global_aspect_definition
    ;
```

#### 4b. New rules to add

Add after existing relevant rules, grouped by RM section:

**RM 4.3.3 — New array aggregate forms:**
```antlr
null_array_aggregate
    : LSB RSB
    ;

array_component_association_list
    : array_component_association (',' array_component_association)*
    ;

iterated_component_association
    : FOR defining_identifier IN discrete_choice_list ARROW expression
    | FOR iterator_specification ARROW expression
    ;
```

**RM 4.3.4 — Delta aggregates:**
```antlr
delta_aggregate
    : record_delta_aggregate
    | array_delta_aggregate
    ;

record_delta_aggregate
    : '(' expression WITH DELTA record_component_association_list ')'
    ;

array_delta_aggregate
    : '(' expression WITH DELTA array_component_association_list ')'
    | LSB expression WITH DELTA array_component_association_list RSB
    ;
```

**RM 4.3.5 — Container aggregates:**
```antlr
container_aggregate
    : null_container_aggregate
    | positional_container_aggregate
    | named_container_aggregate
    ;

null_container_aggregate
    : LSB RSB
    ;

positional_container_aggregate
    : LSB expression (',' expression)* RSB
    ;

named_container_aggregate
    : LSB container_element_association_list RSB
    ;

container_element_association_list
    : container_element_association (',' container_element_association)*
    ;

container_element_association
    : key_choice_list ARROW expression
    | key_choice_list ARROW BOX
    | iterated_element_association
    ;

key_choice_list
    : key_choice (VL key_choice)*
    ;

key_choice
    : expression
    | discrete_range
    ;

iterated_element_association
    : FOR loop_parameter_specification (USE expression)? ARROW expression
    | FOR iterator_specification (USE expression)? ARROW expression
    ;
```

**RM 4.5.9 — Declare expressions:**
```antlr
declare_expression
    : DECLARE declare_item* BEGIN expression
    ;

declare_item
    : object_declaration
    | object_renaming_declaration
    ;
```

**RM 4.5.10 — Reduction attributes:**
```antlr
reduction_attribute_reference
    : value_sequence SQ reduction_attribute_designator
    | name SQ reduction_attribute_designator
    ;

value_sequence
    : LSB (PARALLEL ('(' chunk_specification ')')? aspect_specification?)?
        iterated_element_association RSB
    ;

reduction_attribute_designator
    : identifier '(' reduction_specification ')'
    ;

reduction_specification
    : name COMMA expression
    ;
```

**RM 5.2.1 — Target name:**
```antlr
target_name
    : AT_SIGN
    ;
```

**RM 5.5 — Loop iteration additions:**
```antlr
chunk_specification
    : simple_expression
    | defining_identifier IN discrete_subtype_definition
    ;

iterator_filter
    : WHEN condition
    ;
```

**RM 5.5.2 — Loop parameter subtype:**
```antlr
loop_parameter_subtype_indication
    : subtype_indication
    | access_definition
    ;
```

**RM 5.5.3 — Procedural iterators:**
```antlr
procedural_iterator
    : iterator_parameter_specification OF iterator_procedure_call iterator_filter?
    ;

iterator_parameter_specification
    : formal_part
    | '(' defining_identifier (',' defining_identifier)* ')'
    ;

iterator_procedure_call
    : name
    | name iterator_actual_parameter_part
    ;

iterator_actual_parameter_part
    : '(' iterator_parameter_association (',' iterator_parameter_association)* ')'
    ;

iterator_parameter_association
    : parameter_association
    | parameter_association_with_box
    ;

parameter_association_with_box
    : (selector_name ARROW)? BOX
    ;
```

**RM 5.6.1 — Parallel block statement:**
```antlr
parallel_block_statement
    : PARALLEL ('(' chunk_specification ')')? aspect_specification? DO
        sequence_of_statements
        (AND sequence_of_statements)+
      END DO SEMI
    ;
```

**RM 6.1.2 — Global aspects:**
```antlr
global_aspect_definition
    : NULL_
    | identifier
    | global_mode global_designator
    | '(' global_aspect_element (SEMI global_aspect_element)* ')'
    ;

global_aspect_element
    : global_mode global_set
    | global_mode ALL
    | global_mode SYNCHRONIZED
    ;

global_mode
    : basic_global_mode
    | extended_global_mode
    ;

basic_global_mode
    : IN
    | IN OUT
    | OUT
    ;

global_set
    : global_name (',' global_name)*
    ;

global_designator
    : ALL
    | SYNCHRONIZED
    | global_name
    ;

global_name
    : name
    ;
```

**Annex H.7.1 — Formal parameter / dispatching (SPARK):**
```antlr
extended_global_mode
    : OVERRIDING basic_global_mode
    ;

formal_parameter_set
    : formal_group_designator
    | formal_parameter_name
    | '(' formal_parameter_name (',' formal_parameter_name)* ')'
    ;

formal_group_designator
    : NULL_
    | ALL
    ;

formal_parameter_name
    : name
    ;

dispatching_operation_set
    : dispatching_operation_specifier
    | '(' dispatching_operation_specifier (',' dispatching_operation_specifier)* ')'
    ;

dispatching_operation_specifier
    : name '(' name ')'
    ;
```

---

### Step 5: `IsAggregate()` predicate update in all base classes

In Ada 2022, bracket aggregates (`[...]`) are unambiguous — no predicate needed.
The predicate is only needed for paren-based aggregates, as in Ada 2012.

The existing `IsAggregate()` logic (checks for `WITH` at depth 0) already
handles `delta_aggregate` correctly (since `(expr with delta ...)` contains
`WITH`). No update required to the predicate logic.

All target language base class files are in:
- `CSharp/AdaParserBase.cs`
- `Java/AdaParserBase.java`
- `Python3/AdaParserBase.py`
- `Go/AdaParserBase.go`
- `Antlr4ng/AdaParserBase.ts`
- `JavaScript/AdaParserBase.js`
- `TypeScript/AdaParserBase.ts`
- `Dart/AdaParserBase.dart`
- `Cpp/AdaParserBase.cpp` and `.h`

These are **copied verbatim** from ada2012 with no content changes (only
header comment updates to say "Ada 2022").

---

### Step 6: Test Examples for Ada 2022

Since no official Ada 2022 conformance test suite exists on ada-auth.org,
create example files in `examples/ada2022/`:

| File | Feature Tested |
|------|---------------|
| `target_name.adb` | `@` target name in assignment: `X := @ + 1;` |
| `delta_aggregates.adb` | Record and array delta aggregates: `(R with delta Field => V)` |
| `bracket_aggregates.adb` | Bracket positional/named array aggregates: `[1, 2, 3]` |
| `iterated_components.adb` | Iterated component associations: `(for I in 1..10 => I*I)` |
| `parallel_loops.adb` | `parallel for` and `parallel ... do ... and ... end do` |
| `declare_expressions.adb` | `(declare X : Integer := 5; begin X * 2)` |
| `iterator_filter.adb` | Iterator filter: `for X of Arr when X > 0 loop` |
| `null_array_aggregate.adb` | `A : constant Array_T := [];` |
| `formal_defaults.adb` | `type T is private or use Default_T;` in generic |
| `global_aspects.adb` | `with Global => (in X, out Y)` on a procedure |

Each file should be standalone syntactically (may use `pragma Unreferenced` or
similar to avoid Ada semantic errors, since these are parse-only tests).

Update `desc.xml` to reference the ada2012 examples via a relative path
(so all existing ACATS and semantic tests still run), plus the new ada2022-specific
examples:

```xml
<test>
    <inputs>../ada2012/examples/*.adb</inputs>
</test>
<test>
    <inputs>../ada2012/examples/acats/a/**/*.ada</inputs>
</test>
<test>
    <inputs>../ada2012/examples/semtest/main.adb</inputs>
</test>
<test>
    <inputs>examples/*.adb</inputs>
</test>
```

The `examples/` directory in ada2022 contains only Ada 2022-specific test files.

---

### Step 7: Ambiguity Notes

**`null_array_aggregate` vs `null_container_aggregate`** — Both are `[ ]`.
These two rules are syntactically identical, so they will be merged into a
single `null_bracket_aggregate` rule, or one rule's alternative will cover
both. At parse time, both parse identically; semantic distinction is left to
the semantic analysis layer.

**`reduction_attribute_reference` in `name`** — Since `value_sequence` starts
with `[`, it is unambiguous vs. other `name` alternatives. We add
`reduction_attribute_reference` as a new alternative to `name`.

**`declare_expression`** — `declare` is already a keyword. Inside the `primary`
rule, the existing `name` alternative cannot start with `declare` (it's a
keyword, not an identifier), so `'(' declare_expression ')'` is unambiguous
from `'(' expression ')'`.

**`parallel_block_statement`** — In `iteration_scheme`, `parallel` is now a
reserved word, so `PARALLEL for ...` is unambiguous from `for ...` and `while ...`.

---

### Verification

1. **Build and parse existing Ada 2012 ACATS tests** — Run the grammar against
   `../ada2012/examples/acats/a/**/*.ada` (Ada 2022 is a strict superset;
   these should still pass with zero errors).

2. **Parse new Ada 2022 example files** — Run each file in `examples/`
   through the parser using the desc.xml test runner. Expect zero parse errors.

3. **Run the semtest** — `../ada2012/examples/semtest/main.adb` should still
   parse correctly (no Ada 2022-specific changes break it).
