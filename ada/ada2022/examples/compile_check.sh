#!/usr/bin/env bash
# Compile all Ada 2022 examples with GNAT to verify they are accepted.
# parallel_loops.adb is skipped: GNAT 15 does not yet implement parallel loops.

set -euo pipefail

GNAT_FLAGS="-gnat2022 -gnatwa"
PASS=0
FAIL=0

compile() {
    local file=$1
    printf "%-40s" "$file"
    if gnatmake $GNAT_FLAGS "$file" >/dev/null 2>&1; then
        echo "PASS"
        PASS=$((PASS + 1))
    else
        echo "FAIL"
        gnatmake $GNAT_FLAGS "$file" 2>&1 | sed 's/^/  /'
        FAIL=$((FAIL + 1))
    fi
}

cd "$(dirname "$0")"

# Standalone procedures (produce executables)
compile target_name.adb
compile delta_aggregates.adb
compile bracket_aggregates.adb
compile iterated_components.adb
compile declare_expressions.adb
compile iterator_filter.adb
compile null_array_aggregate.adb

# Packages and generic units (compile only, no executable)
compile formal_defaults.adb
compile use_formal_incomplete.adb
compile global_aspects.adb

echo ""
echo "Results: $PASS passed, $FAIL failed"
[ $FAIL -eq 0 ]
