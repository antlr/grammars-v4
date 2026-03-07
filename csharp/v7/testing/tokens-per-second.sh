#!/usr/bin/env bash

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_URL="https://github.com/dotnet/roslyn.git"
TAG="version-2.8.2"
ROSLYN_DIR="$SCRIPT_DIR/roslyn"
GENERATED_DIR="$SCRIPT_DIR/../Generated-CSharp"
TEST_SUBDIR="src/Compilers/CSharp/Test"
TEST_EXE="$(find "$GENERATED_DIR/bin" -name "Test.exe" 2>/dev/null | sort | tail -1 || true)"
if [[ -z "$TEST_EXE" ]]; then
    echo "Error: Test.exe not found under $GENERATED_DIR/bin" >&2
    exit 1
fi
p=`cygpath -u "$ROSLYN_DIR/src/"`

tokens=`find "$p" -name '*.cs' | \
    grep -v 'Metadata/public-and-private.cs' | \
    grep -v 'VisualStudioInstanceFactory.cs' | \
    while IFS= read -r f; do cygpath -w "$f"; done | \
    "$TEST_EXE" -x -tc 2>&1 | \
    grep -e '^TC:' | \
    awk '{print $2}'`

echo "tokens = $tokens"

times=()
for i in 1 2 3 4 5; do
    t=`find "$p" -name '*.cs' | \
        grep -v 'Metadata/public-and-private.cs' | \
        grep -v 'VisualStudioInstanceFactory.cs' | \
        while IFS= read -r f; do cygpath -w "$f"; done | \
        "$TEST_EXE" -x 2>&1 | \
        grep -e '^Total Time:' | \
        awk '{print $3}'`
    echo "  run $i: $t s"
    times+=("$t")
done

OCTAVE="octave"
if ! command -v octave &>/dev/null; then
    OCTAVE="/C/Program Files/GNU Octave/Octave-7.2.0/mingw64/bin/octave"
fi

times_str="${times[*]}"
read mean_tps std_tps < <("$OCTAVE" --no-gui --norc --eval "
  t = [$times_str];
  tps = $tokens ./ t;
  m = mean(tps);
  s = std(tps);
  printf('%d %d\n', round(m/1000)*1000, round(s/1000)*1000);
" 2>/dev/null | tail -1)

echo "Parse of \`testing/roslyn/src/**/*.cs\` is $mean_tps +/- $std_tps tokens per second (SD). Sample size ${#times[@]}, port CSharp."

