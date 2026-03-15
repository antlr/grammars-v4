#!/bin/bash
# generate-csharp-parser.sh
#
# Standalone script — does NOT need to live inside the csharpstandard repo.
# Attach to a GitHub Issue comment and run from any working directory.
#
# What it does:
#   1. Clones https://github.com/dotnet/csharpstandard (or reuses existing clone)
#   2. Installs the BuildGrammar tool bundled with the repo
#   3. Extracts GrammarTestingEnv and creates a CSharpTarget modification set:
#        - Updates ANTLR reference from 4.9.2 to 4.13.2
#        - Provides empty stub Lexer_Support.g4 / Parser_Support.g4 so BuildGrammar
#          produces clean grammars with no @members blocks
#        - Fixes the prog start rule: proper EOF token, no post-parse tree manipulation
#        - Fixes PP_Start: replaces Java getCharPositionInLine() with C# Column property
#   4. Runs BuildGrammar to produce CSharpLexer.g4 / CSharpParser.g4
#   4.5 Post-processes the generated .g4 files:
#        - Adds options { superClass=CSharpLexerBase; } to CSharpLexer.g4
#        - Adds options { superClass=CSharpParserBase; } to CSharpParser.g4
#        - Removes the empty Lexer_Support.g4 / Parser_Support.g4 stubs from output
#        - Writes CSharpLexerBase.cs and CSharpParserBase.cs alongside the .g4 files
#   5. Generates a complete C# parser project via trgen, then copies the base-class
#      .cs files into the generated project before building
#   6. Builds the project with dotnet
#
# Usage:
#   bash generate-csharp-parser.sh [output-dir] [repo-dir]
#
#   output-dir  where the .g4 files and built parser land
#               default: ./csharp-parser
#   repo-dir    where to clone / find the csharpstandard repo
#               default: ./csharpstandard
#
# Prerequisites: dotnet (>= 8.0), git, bash
# Java / ANTLR jar: NOT required for the C# target workflow.

set -euo pipefail

REPO_URL="https://github.com/dotnet/csharpstandard.git"
OUTPUT_DIR="$(pwd)/${1:-csharp-parser}"
REPO_DIR="$(pwd)/${2:-csharpstandard}"

echo "==> C# Standard grammar → ANTLR4 C# parser"
echo "    Repo:        $REPO_DIR"
echo "    Output:      $OUTPUT_DIR"
echo ""

# ─── 1. Clone (or reuse) the csharpstandard repo ─────────────────────────────
if [ -d "$REPO_DIR/.git" ]; then
    echo "[1/6] Using existing clone at $REPO_DIR"
else
    echo "[1/6] Cloning $REPO_URL..."
    git clone --depth 1 "$REPO_URL" "$REPO_DIR"
fi

DEPS_DIR="$REPO_DIR/.github/workflows/dependencies"
STANDARD_DIR="$REPO_DIR/standard"

# ─── 2. Install BuildGrammar ──────────────────────────────────────────────────
echo "[2/6] Installing BuildGrammar tool..."
dotnet tool install --version 2.1.0 --global \
    --add-source "$DEPS_DIR" EcmaTC49.BuildGrammar 2>/dev/null \
  || dotnet tool update --version 2.1.0 --global \
    --add-source "$DEPS_DIR" EcmaTC49.BuildGrammar 2>/dev/null \
  || true   # already at correct version is fine

# Ensure ~/.dotnet/tools is on PATH (needed after a first-time global install)
export PATH="$PATH:$HOME/.dotnet/tools"

# ─── 3. Set up patched GrammarTestingEnv ─────────────────────────────────────
echo "[3/6] Creating CSharpTarget modification set..."
WORK_DIR=$(mktemp -d)
trap 'rm -rf "$WORK_DIR"' EXIT

cd "$WORK_DIR"
tar -xzf "$DEPS_DIR/GrammarTestingEnv.tgz"

MS="$WORK_DIR/Environment/ModificationSets"

# Update ANTLR version reference in the environment setup script
sed -i 's/antlr-4\.9\.2-complete\.jar/antlr-4.13.2-complete.jar/g' \
    Environment/Antlr/_setupEnvironment

# ─── Build CSharpTarget modification set ──────────────────────────────────────
mkdir -p "$MS/CSharpTarget"

# Shared files are identical to Base
cp "$MS/Base/InterpolatedSupport.md" \
   "$MS/Base/Macros.json" \
   "$MS/Base/_Modifications_Order.txt" \
   "$MS/CSharpTarget/"

# Copy and patch ReplaceAndAdd.md:
#   • Fix prog start rule — replace semantic-predicate EOF check and post-parse
#     tree manipulation call with a plain EOF token (correct ANTLR >= 4.6 form)
#   • Add PP_Start fix — replace Java getCharPositionInLine() with C# Column
cp "$MS/Base/ReplaceAndAdd.md" "$MS/CSharpTarget/"

sed -i \
    's/compilation_unit LookAheadIs«EOF» ReduceTree«»/compilation_unit EOF/' \
    "$MS/CSharpTarget/ReplaceAndAdd.md"

cat >> "$MS/CSharpTarget/ReplaceAndAdd.md" << 'PPSTART_MD'

---

## C# Target: PP_Start

The Standard's `PP_Start` rule contains `{ getCharPositionInLine() == 0 }?`, which is
Java-specific ANTLR action syntax. The spec explicitly notes this predicate is
informative only. Replace with the equivalent C# ANTLR4 runtime `Column` property.

### 6.5.1 General

```ANTLR
// [C# TARGET] Replace Java getCharPositionInLine() with C# Column property
# Modify
fragment PP_Start
    : { Column == 0 }? PP_Whitespace? '#' PP_Whitespace?
    ;
```
PPSTART_MD

# ─── Lexer_Support.g4 stub ───────────────────────────────────────────────────
# Empty stub so BuildGrammar produces a clean CSharpLexer.g4 with no @members.
# All lexer helper methods live in CSharpLexerBase.cs (superClass = CSharpLexerBase).
cat > "$MS/CSharpTarget/Lexer_Support.g4" << 'LEXER_SUPPORT'
lexer grammar Lexer_Support;

// Lexer helper methods are provided by CSharpLexerBase.cs.
// The generated CSharpLexer will extend CSharpLexerBase via
//   options { superClass = CSharpLexerBase; }
LEXER_SUPPORT

# ─── Parser_Support.g4 stub ──────────────────────────────────────────────────
# Empty stub so BuildGrammar produces a clean CSharpParser.g4 with no @members.
# All parser helper methods live in CSharpParserBase.cs (superClass = CSharpParserBase).
cat > "$MS/CSharpTarget/Parser_Support.g4" << 'PARSER_SUPPORT'
parser grammar Parser_Support;

// Parser helper methods are provided by CSharpParserBase.cs.
// The generated CSharpParser will extend CSharpParserBase via
//   options { superClass = CSharpParserBase; }
PARSER_SUPPORT

# ─── 4. Run BuildGrammar ──────────────────────────────────────────────────────
echo "[4/6] Running BuildGrammar (CSharpTarget modification set)..."
mkdir -p "$OUTPUT_DIR"
dotnet csharpgrammar "$OUTPUT_DIR" CSharp "$STANDARD_DIR" \
    -ms "$MS/CSharpTarget"
echo "      Written: $(ls "$OUTPUT_DIR"/*.g4 | xargs -n1 basename | tr '\n' ' ')"

# ─── 4.5. Post-process generated .g4 files + write C# base classes ───────────
echo "[4.5/6] Patching .g4 files and writing CSharpLexerBase.cs / CSharpParserBase.cs..."

# Insert superClass=... into the grammar's options block.
# If an options { } block already exists (e.g. CSharpParser has tokenVocab=...),
# the new entry is merged inside it.  Otherwise a new block is added after the
# grammar declaration line.  Handles both single-line and multi-line forms.
# Uses grep + awk so it works on both Linux and MSYS2 without GNU-sed extensions.
patch_g4_superclass() {
    local file="$1" superclass="$2"
    if grep -q 'options[[:space:]]*{' "$file"; then
        # An options block already exists — merge superClass into it.
        awk -v sc="$superclass" '
            /options[[:space:]]*\{/ && /\}/ && !done {
                # Single-line: options { tokenVocab=...; }
                sub(/\}/, "superClass=" sc "; }")
                done=1
                print
                next
            }
            /options[[:space:]]*\{/ && !done {
                # Opening line of a multi-line options block.
                in_options=1
                print
                next
            }
            in_options && /\}/ {
                # Closing brace of the multi-line options block.
                print "    superClass=" sc ";"
                in_options=0
                done=1
            }
            { print }
        ' "$file" > "${file}.tmp" && mv "${file}.tmp" "$file"
    else
        # No options block yet — insert one after the grammar declaration line.
        awk -v sc="$superclass" '
            /^(lexer|parser) grammar / && !done {
                print
                print ""
                print "options { superClass=" sc "; }"
                print ""
                done=1
                next
            }
            { print }
        ' "$file" > "${file}.tmp" && mv "${file}.tmp" "$file"
    fi
}

patch_g4_superclass "$OUTPUT_DIR/CSharpLexer.g4"  "CSharpLexerBase"
patch_g4_superclass "$OUTPUT_DIR/CSharpParser.g4" "CSharpParserBase"

# Remove the (now-empty) support stubs from the output directory; they are no
# longer needed and would confuse trgen if left alongside the main grammars.
rm -f "$OUTPUT_DIR/Lexer_Support.g4" "$OUTPUT_DIR/Parser_Support.g4"

# ─── CSharpLexerBase.cs ───────────────────────────────────────────────────────
cat > "$OUTPUT_DIR/CSharpLexerBase.cs" << 'CSHARP_LEXER_BASE'
using System;
using System.IO;
using Antlr4.Runtime;

/// <summary>
/// Base class for the generated CSharpLexer.
/// Contains all helper methods previously written as @lexer::members in
/// Lexer_Support.g4.  The generated lexer opts in via:
///   options { superClass = CSharpLexerBase; }
///
/// Key C# / Java runtime API differences reflected here:
///   _modeStack         → ModeStack  (public Stack&lt;int&gt; property)
///   _input.LA()        → InputStream.LA()
/// </summary>
public abstract class CSharpLexerBase : Lexer
{
    protected CSharpLexerBase(ICharStream input)
        : base(input) { }

    protected CSharpLexerBase(ICharStream input, TextWriter output, TextWriter errorOutput)
        : base(input, output, errorOutput) { }

    // ANTLR provides PushMode/PopMode/CurrentMode but no PeekMode.
    // Peek at the top of the mode stack without popping it.
    public int peekMode()
    {
        return ModeStack.Count == 0 ? DEFAULT_MODE : ModeStack.Peek();
    }

    // Override PopMode to make the stack bottomless: mismatched brackets
    // produce a parser-level error rather than a stack underflow exception.
    public override int PopMode()
    {
        if (ModeStack.Count == 0)
        {
            Console.Error.WriteLine("unbalanced ()/{}/[]");
            return DEFAULT_MODE;
        }
        return base.PopMode();
    }

    // Convenience predicates — names must match Macros.json expansions.
    public bool peekModeIs(int mode) => peekMode() == mode;

    public bool lookAheadIs(int pos, int value)    => InputStream.LA(pos) == value;
    public bool lookAheadIsNot(int pos, int value) => InputStream.LA(pos) != value;

    // Wrap an interpolated-string fragment token in Unicode tortoise brackets
    // U+3014 〔 and U+3015 〕 so that grun tree output remains parseable.
    public void wrapToken()
    {
        Text = "\u3014" + Text.Replace("\u3015", "\u3015\u3015") + "\u3015";
    }
}
CSHARP_LEXER_BASE

# ─── CSharpParserBase.cs ──────────────────────────────────────────────────────
cat > "$OUTPUT_DIR/CSharpParserBase.cs" << 'CSHARP_PARSER_BASE'
using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Reflection;
using Antlr4.Runtime;
using Antlr4.Runtime.Tree;

/// <summary>
/// Base class for the generated CSharpParser.
/// Contains all helper methods previously written as @parser::members in
/// Parser_Support.g4.  The generated parser opts in via:
///   options { superClass = CSharpParserBase; }
///
/// Key C# / Java runtime API differences reflected here:
///   _input.LA()          → InputStream.LA()
///   SyntaxError(...)     → SyntaxError(TextWriter, ...)  — extra TextWriter arg
///   setParent() calls    → omitted (ANTLR manages parent links for normal traversal)
///
/// Rule-index constants (RULE_*) and token-type constants (TK_*) are defined in
/// the generated CSharpParser / CSharpLexer.  From this base class they are
/// resolved by name:
///   RULE_foo  → Array.IndexOf(RuleNames, "foo")
///   TK_BAR    → TokenTypeMap["TK_BAR"]
///
/// Context types (Invocation_expressionContext, etc.) are also generated.  The
/// as*() insertion helpers locate their constructors at runtime via a one-time
/// assembly scan cached in _ctorCache.
/// </summary>
public abstract class CSharpParserBase : Parser
{
    protected CSharpParserBase(ITokenStream input)
        : base(input) { }

    protected CSharpParserBase(ITokenStream input, TextWriter output, TextWriter errorOutput)
        : base(input, output, errorOutput) { }

    //==========================================================================
    // Semantic error reporting
    //==========================================================================

    private void notifySemanticError(int line, int charPositionInLine, string msg)
    {
        IAntlrErrorListener<IToken> listener = ErrorListenerDispatch;
        listener.SyntaxError(Console.Error, this, null, line, charPositionInLine, msg, null);
    }

    //==========================================================================
    // Convenience predicates — names must match Macros.json expansions.
    //==========================================================================

    public bool lookAheadIs(int pos, int value)    => InputStream.LA(pos) == value;
    public bool lookAheadIsNot(int pos, int value) => InputStream.LA(pos) != value;

    //==========================================================================
    // Rule / token index helpers
    //==========================================================================

    private new int GetRuleIndex(string ruleName) => Array.IndexOf(RuleNames, ruleName);

    private new int GetTokenType(string tokenName)
        => TokenTypeMap.TryGetValue(tokenName, out var t) ? t
           : throw new KeyNotFoundException($"Token type '{tokenName}' not found in vocabulary.");

    //==========================================================================
    // Post-parse tree cleanup
    //   Pass 1: Remove empty type_argument_list nodes — artifacts of the '<'
    //           disambiguation predicate.
    //   Pass 2: Optionally collapse single-parent/child chains (off by default;
    //           set env var ANTLR_REDUCE_TREE=yes to enable).
    //==========================================================================

    public void reduceTree(ParserRuleContext currentctx)
    {
        int ruleTypeArgumentList = GetRuleIndex("type_argument_list");

        void takeOutEmpties(IParseTree node)
        {
            if (node is ParserRuleContext parent && parent.children != null)
            {
                for (int ix = parent.children.Count - 1; ix >= 0; ix--)
                {
                    var anyTarget = parent.children[ix];
                    if (anyTarget is ParserRuleContext childTarget)
                    {
                        if (childTarget.RuleIndex == ruleTypeArgumentList
                            && childTarget.ChildCount == 0)
                            parent.children.RemoveAt(ix);
                        else
                            takeOutEmpties(childTarget);
                    }
                }
            }
        }
        takeOutEmpties(currentctx);

        string reduceProperty = (Environment.GetEnvironmentVariable("ANTLR_REDUCE_TREE") ?? "no").ToLowerInvariant();
        if (reduceProperty != "yes") return;

        string reduceAllChildrenProperty = (Environment.GetEnvironmentVariable("ANTLR_REDUCE_ALL_CHILDREN") ?? "no").ToLowerInvariant();
        bool reduceAllChildren = reduceAllChildrenProperty == "yes";

        void reducer(IParseTree node)
        {
            if (node is ParserRuleContext parent && parent.children != null)
            {
                foreach (var child in parent.children)
                    reducer(child);
                if (reduceAllChildren || parent.ChildCount == 1)
                {
                    for (int i = 0; i < parent.children.Count; i++)
                    {
                        var child = parent.children[i];
                        if (child is ParserRuleContext childNode && childNode.ChildCount == 1)
                        {
                            var grandchild = childNode.GetChild(0);
                            if (grandchild is ParserRuleContext grandchildNode)
                                parent.children[i] = grandchildNode;
                        }
                    }
                }
            }
        }
        if (currentctx.children != null)
            reducer(currentctx);
    }

    //==========================================================================
    // Helpers to "undo" MLR inlining in primary_expression:
    // Moves all current children of currentctx into a new child context of the
    // appropriate type, restoring parse tree nodes elided by MLR removal.
    //
    // The generated context types (Invocation_expressionContext, etc.) are not
    // visible from this base class.  _ctorCache is built once by scanning the
    // assembly for ParserRuleContext subclasses and caching their (parent, state)
    // constructors — the same assembly that contains this base class also
    // contains all generated parser types.
    //==========================================================================

    private static Dictionary<string, ConstructorInfo> _ctorCache;

    private static Dictionary<string, ConstructorInfo> BuildCtorCache()
    {
        var map = new Dictionary<string, ConstructorInfo>(StringComparer.Ordinal);
        foreach (var t in typeof(CSharpParserBase).Assembly.GetTypes())
        {
            if (!t.IsSubclassOf(typeof(ParserRuleContext))) continue;
            var ctor = t.GetConstructor(new[] { typeof(ParserRuleContext), typeof(int) });
            if (ctor != null) map[t.Name] = ctor;
        }
        return map;
    }

    private void insertNode(ParserRuleContext currentctx, ParserRuleContext insertedctx)
    {
        insertedctx.children = currentctx.children != null
            ? new List<IParseTree>(currentctx.children)
            : new List<IParseTree>();
        if (currentctx.children == null)
            currentctx.children = new List<IParseTree>();
        currentctx.children.Clear();
        currentctx.children.Add(insertedctx);
    }

    private void insertNodeByTypeName(ParserRuleContext currentctx, string typeName)
    {
        if (_ctorCache == null) _ctorCache = BuildCtorCache();
        if (!_ctorCache.TryGetValue(typeName, out var ctor))
            throw new InvalidOperationException(
                $"Cannot find a (ParserRuleContext, int) constructor for type '{typeName}'.");
        var ctx = (ParserRuleContext)ctor.Invoke(new object[] { currentctx, currentctx.invokingState });
        insertNode(currentctx, ctx);
    }

    public void asInvocationExpression(ParserRuleContext currentctx)
        => insertNodeByTypeName(currentctx, "Invocation_expressionContext");

    public void asElementAccess(ParserRuleContext currentctx)
        => insertNodeByTypeName(currentctx, "Element_accessContext");

    public void asMemberAccess(ParserRuleContext currentctx)
        => insertNodeByTypeName(currentctx, "Member_accessContext");

    public void asNullConditionalMemberAccess(ParserRuleContext currentctx)
        => insertNodeByTypeName(currentctx, "Null_conditional_member_accessContext");

    public void asNullConditionalElementAccess(ParserRuleContext currentctx)
        => insertNodeByTypeName(currentctx, "Null_conditional_element_accessContext");

    public void asPostIncrementExpression(ParserRuleContext currentctx)
        => insertNodeByTypeName(currentctx, "Post_increment_expressionContext");

    public void asPostDecrementExpression(ParserRuleContext currentctx)
        => insertNodeByTypeName(currentctx, "Post_decrement_expressionContext");

    public void asNullForgivingExpression(ParserRuleContext currentctx)
        => insertNodeByTypeName(currentctx, "Null_forgiving_expressionContext");

    public void asPointerMemberAccess(ParserRuleContext currentctx)
        => insertNodeByTypeName(currentctx, "Pointer_member_accessContext");

    //==========================================================================
    // Semantic check from §12.8.11-2:
    // array_creation_expression[...] and stackalloc_expression[...] must have
    // an initializer on the LHS of an element access.
    //==========================================================================

    public void elementAccessSemanticCheck(ParserRuleContext currentctx)
    {
        int rPrimary         = GetRuleIndex("primary_expression");
        int rElemAccess      = GetRuleIndex("element_access");
        int rPtrElemAccess   = GetRuleIndex("pointer_element_access");
        int rNullCondElem    = GetRuleIndex("null_conditional_element_access");
        int rArrayCreation   = GetRuleIndex("array_creation_expression");
        int rStackalloc      = GetRuleIndex("stackalloc_expression");
        int tkRBrace         = GetTokenType("TK_RBRACE");
        int tkRBrack         = GetTokenType("TK_RBRACK");

        if (currentctx.RuleIndex != rPrimary || currentctx.ChildCount != 1)
            return;

        var childTree = currentctx.GetChild(0);
        if (childTree is not ParserRuleContext childRule) return;

        int childRuleIndex  = childRule.RuleIndex;
        int childChildCount = childRule.ChildCount;

        if (childRuleIndex == rElemAccess || childRuleIndex == rPtrElemAccess)
        {
            if (childChildCount != 4) return;
        }
        else if (childRuleIndex == rNullCondElem)
        {
            if (childChildCount != 5) return;
        }
        else
        {
            return;
        }

        var accessTargetTree      = childRule.GetChild(0);
        var tokenForErrorLocation = childRule.GetChild(1);

        if (accessTargetTree is not ParserRuleContext accessTarget) return;
        if (accessTarget.RuleIndex != rPrimary || accessTarget.ChildCount == 0) return;

        var lhsTargetTree = accessTarget.GetChild(0);
        if (lhsTargetTree is not ParserRuleContext lhsTarget) return;

        if (lhsTarget.RuleIndex != rArrayCreation && lhsTarget.RuleIndex != rStackalloc)
            return;

        IToken lhsLast     = lhsTarget.Stop;
        int    lhsLastType = lhsLast.Type;
        string lhsLastText = lhsLast.Text;

        if (lhsLastType == tkRBrace) return; // initializer present — check passes

        if (lhsLastType != tkRBrack)
        {
            Console.Error.WriteLine(
                $"{lhsLast.Line}:{lhsLast.Column} Error: Unexpected LHS last token {lhsLastText} ({lhsLastType}).");
            return;
        }

        IToken reportAt    = tokenForErrorLocation is TerminalNodeImpl tni ? tni.Symbol : lhsLast;
        string childRuleName = RuleNames[childRuleIndex];
        string lhsRuleName   = RuleNames[lhsTarget.RuleIndex];
        string childPrefix   = "AEIOUaeiou".IndexOf(childRuleName[0]) != -1 ? "an" : "a";
        string lhsPrefix     = "AEIOUaeiou".IndexOf(lhsRuleName[0])   != -1 ? "an" : "a";

        notifySemanticError(reportAt.Line, reportAt.Column,
            $"LHS of {childPrefix} {childRuleName} cannot be {lhsPrefix} {lhsRuleName} unless it has an initializer");
    }
}
CSHARP_PARSER_BASE

echo "      Written: CSharpLexerBase.cs, CSharpParserBase.cs"

# ─── 5. Generate C# parser project via trgen ──────────────────────────────────
echo "[5/6] Generating C# parser project with trgen..."
cd "$OUTPUT_DIR"

# Install trgen and trxml2 globally if not already available
# (trxml2 is used by the generated build.sh to extract the ANTLR runtime version)
for tool in trgen trxml2; do
    if ! command -v $tool > /dev/null 2>&1; then
        echo "      Installing $tool..."
        dotnet tool install -g $tool
    fi
done

trgen -t CSharp

# trgen's build.sh calls "dotnet trxml2" which only works with a local tool manifest.
# Since we install trxml2 as a global tool, patch to call it directly instead.
sed -i 's/dotnet trxml2/trxml2/g' Generated-CSharp/build.sh

# Copy the base-class files into the generated project so they are compiled
# as part of the same assembly as the generated CSharpParser / CSharpLexer.
cp "$OUTPUT_DIR/CSharpLexerBase.cs"  Generated-CSharp/
cp "$OUTPUT_DIR/CSharpParserBase.cs" Generated-CSharp/

# ─── 6. Build ─────────────────────────────────────────────────────────────────
echo "[6/6] Building..."
cd Generated-CSharp
bash build.sh

echo ""
echo "==> Success."
echo "    Grammar files : $OUTPUT_DIR/*.g4"
echo "    Base classes  : $OUTPUT_DIR/Generated-CSharp/CSharpLexerBase.cs"
echo "                    $OUTPUT_DIR/Generated-CSharp/CSharpParserBase.cs"
echo "    Parser binary : $OUTPUT_DIR/Generated-CSharp/bin/Debug/net*/Test"
echo ""
echo "    To parse a C# file:"
echo "      cat file.cs | $OUTPUT_DIR/Generated-CSharp/bin/Debug/net*/Test"
