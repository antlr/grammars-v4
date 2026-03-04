#!/usr/bin/env bash
# test.sh — Clone Roslyn at the C# 7.3 tag, extract C# source snippets from
# the compiler test suite, and verify each one parses cleanly with the ANTLR
# grammar built in ../Generated-CSharp.
#
# Pre-requisites (run once from csharp/v7/ before calling this script):
#   dotnet trgen -t CSharp
#   cd Generated-CSharp && bash build.sh
#
# Usage:  bash test.sh

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

REPO_URL="https://github.com/dotnet/roslyn.git"
TAG="version-2.8.2"
ROSLYN_DIR="$SCRIPT_DIR/roslyn"
GENERATED_DIR="$SCRIPT_DIR/../Generated-CSharp"
TEST_SUBDIR="src/Compilers/CSharp/Test"

echo "=== Roslyn C# 7.3 Grammar Test ==="
echo "Tag : $TAG"
echo

# ── 1. clone Roslyn ───────────────────────────────────────────────────────────
echo "[1/5] Roslyn source"
if [ ! -d "$ROSLYN_DIR" ]; then
    echo "  Cloning $REPO_URL ..."
    git clone --branch "$TAG" --depth 1 "$REPO_URL" "$ROSLYN_DIR"
else
    echo "  Using existing clone: $ROSLYN_DIR"
fi

# ── 2. ensure ripgrep ─────────────────────────────────────────────────────────
echo
echo "[2/5] ripgrep"
# The UCRT64 package installs to /c/msys64/ucrt64/bin which may not be in PATH
UCRT64_BIN="/c/msys64/ucrt64/bin"
if [ -f "$UCRT64_BIN/rg.exe" ] && ! command -v rg &>/dev/null; then
    export PATH="$UCRT64_BIN:$PATH"
fi
if ! command -v rg &>/dev/null; then
    echo "  Not found — installing mingw-w64-ucrt-x86_64-ripgrep via pacman..."
    pacman -S --noconfirm mingw-w64-ucrt-x86_64-ripgrep
    export PATH="$UCRT64_BIN:$PATH"
fi
echo "  $(rg --version | head -1)"

# ── 3. locate ANTLR test executable ──────────────────────────────────────────
echo
echo "[3/5] ANTLR test executable"
TEST_EXE="$(find "$GENERATED_DIR/bin" -name "Test.exe" 2>/dev/null | sort | tail -1 || true)"
if [ -z "$TEST_EXE" ]; then
    cat >&2 <<MSG
  error: Test.exe not found under $GENERATED_DIR/bin
  Build it first:
    cd "$SCRIPT_DIR/.."
    dotnet trgen -t CSharp
    cd Generated-CSharp && bash build.sh
MSG
    exit 1
fi
echo "  $TEST_EXE"

# ── 4. extract C# 7.3 snippets ────────────────────────────────────────────────
echo
echo "[4/5] Extracting C# snippets from Roslyn tests"

# 4a. test files that reference LangVersion.CSharp7_3
mapfile -t LANG7_FILES < <(
    rg --type cs -l \
        'LangVersion\.CSharp7_3|LanguageVersion\.CSharp7_3' \
        "$ROSLYN_DIR/$TEST_SUBDIR" 2>/dev/null || true
)
echo "  Files referencing LangVersion.CSharp7_3 : ${#LANG7_FILES[@]}"

# 4b. narrow to files that also contain test call sites
if [ "${#LANG7_FILES[@]}" -gt 0 ]; then
    mapfile -t CALL_FILES < <(
        rg -l 'Parse\(|CreateCompilation\(|CompileAndVerify\(' \
            "${LANG7_FILES[@]}" 2>/dev/null || true
    )
else
    CALL_FILES=()
fi
echo "  Of those, with Parse/CreateCompilation/CompileAndVerify: ${#CALL_FILES[@]}"

WORK_DIR="$(mktemp -d)"
trap 'rm -rf "$WORK_DIR"' EXIT

if [ "${#CALL_FILES[@]}" -gt 0 ]; then
    # Write file list to a temp file to avoid command-line length limits
    LIST_FILE="$WORK_DIR/filelist.txt"
    printf '%s\n' "${CALL_FILES[@]}" > "$LIST_FILE"

    SNIPPET_COUNT="$(
        python3 - "$WORK_DIR" "$LIST_FILE" <<'PYEOF'
import sys, re, os

work_dir  = sys.argv[1]
list_file = sys.argv[2]

with open(list_file) as lf:
    files = [line.rstrip('\n') for line in lf if line.strip()]

# @"..." verbatim string literals (handle "" escape inside)
VERBATIM = re.compile(r'@"((?:[^"]|"")*)"', re.DOTALL)

# Heuristic: string looks like compilable C# source
CS_SRC = re.compile(
    r'\b(?:class|struct|interface|enum|namespace|delegate)\s+\w'
    r'|\b(?:void|int|string|bool|object|var)\s+\w'
    r'|\busing\s+\w'
    r'|\b(?:public|private|internal|protected|static)\s+\w'
)
# Reject IL disassembly (CompileAndVerify checks expected IL output)
IL_ASM = re.compile(r'\bIL_[0-9a-f]{4}:|\.maxstack\b|\.locals\b')
# Reject compiler diagnostic output strings (not source)
DIAG_OUT = re.compile(r'\(\d+,\d+\): (error|warning) CS\d+')
# Reject format-string placeholders like {pointerType} that appear after
# '=' or ':' — positions where '{identifier}' is never valid C# syntax.
# (Avoids false positives from '{name}' inside C# string literals.)
FMT_PLACEHOLDER = re.compile(r'(?:=|:)\s*\{[A-Za-z_]\w*\}')
# Reject intentionally-invalid 'using Foo*' (wildcard namespace — not C#)
USING_STAR = re.compile(r'\busing\s+\w[\w.]*\s*\*')

count = 0
for path in files:
    try:
        text = open(path, encoding='utf-8', errors='replace').read()
    except OSError:
        continue
    for m in VERBATIM.finditer(text):
        raw = m.group(1)
        # Decode verbatim-string escapes: "" -> " and {{ -> { / }} -> }
        snippet = raw.replace('""', '"').replace('{{', '{').replace('}}', '}').strip()
        if len(snippet) < 15:
            continue
        if not CS_SRC.search(snippet):
            continue
        if IL_ASM.search(snippet):
            continue
        if DIAG_OUT.search(snippet):
            continue
        # Reject code fragments: compilation_unit never starts with '{' or '}'
        if snippet[0] in ('{', '}'):
            continue
        if FMT_PLACEHOLDER.search(snippet):
            continue
        if USING_STAR.search(snippet):
            continue
        out_path = os.path.join(work_dir, f'snippet_{count:05d}.cs')
        with open(out_path, 'w', encoding='utf-8') as fh:
            fh.write(snippet + '\n')
        count += 1

print(count)
PYEOF
    )"
    echo "  Snippets extracted: $SNIPPET_COUNT"
else
    echo "  No qualifying test files found."
fi

# Collect snippet files
shopt -s nullglob
SNIPPETS=("$WORK_DIR"/snippet_*.cs)
shopt -u nullglob

if [ "${#SNIPPETS[@]}" -eq 0 ]; then
    echo "  Nothing to test."
    exit 0
fi

# ── 5. parse with ANTLR grammar ───────────────────────────────────────────────
echo
echo "[5/5] Parsing ${#SNIPPETS[@]} snippets with ANTLR grammar"

pass=0
fail=0
declare -a FAILED_LIST=()
total="${#SNIPPETS[@]}"
idx=0

for snippet in "${SNIPPETS[@]}"; do
    idx=$((idx + 1))
    if (( idx % 100 == 0 )) || (( idx == total )); then
        printf "  %d/%d ...\n" "$idx" "$total"
    fi
    if "$TEST_EXE" "$snippet" > /dev/null 2>&1; then
        pass=$((pass + 1))
    else
        fail=$((fail + 1))
        FAILED_LIST+=("$snippet")
    fi
done

echo
echo "Results: $pass passed, $fail failed (total: $((pass + fail)))"

if [ "${#FAILED_LIST[@]}" -gt 0 ]; then
    echo
    echo "=== Parse failures (first 20 shown) ==="
    shown=0
    for s in "${FAILED_LIST[@]}"; do
        (( shown < 20 )) || break
        shown=$((shown + 1))
        echo "──── $(basename "$s") ────"
        "$TEST_EXE" "$s" 2>&1 | grep -m 3 'line [0-9]' || true
        echo "  Source (first 8 lines):"
        head -8 "$s" | sed 's/^/    /'
        echo
    done
    exit 1
fi
