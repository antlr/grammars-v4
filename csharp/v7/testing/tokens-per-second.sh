#!/usr/bin/env bash

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_URL="https://github.com/dotnet/roslyn.git"
TAG="version-2.8.2"
ROSLYN_DIR="$SCRIPT_DIR/roslyn"
GENERATED_DIR="$SCRIPT_DIR/../Generated-CSharp"
TEST_SUBDIR="src/Compilers/CSharp/Test"
TEST_EXE="$(find "$GENERATED_DIR/bin" -name "Test.exe" 2>/dev/null | sort | tail -1 || true)"
p=`cygpath -u "$ROSLYN_DIR/src/"`

tokens=`find "$p" -name '*.cs' | \
    grep -v 'Metadata/public-and-private.cs' | \
    grep -v 'VisualStudioInstanceFactory.cs' | \
    while IFS= read -r f; do cygpath -w "$f"; done | \
    "$TEST_EXE" -x -tc 2>&1 | \
    grep -e '^TC:' | \
    awk '{print $2}'`

echo tokens = $tokens

for i in 1
do
    find "$p" -name '*.cs' | \
        grep -v 'Metadata/public-and-private.cs' | \
        grep -v 'VisualStudioInstanceFactory.cs' | \
        while IFS= read -r f; do cygpath -w "$f"; done | \
        "$TEST_EXE" -x 2>&1 | \
        grep -e '^Total Time:' | \
	awk '{print $3}'
done
