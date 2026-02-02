#!/bin/bash
#
# find-stale-link-grammars.sh - Check for stale links in changed grammars
#
# Usage: find-stale-link-grammars.sh [BEFORE_SHA AFTER_SHA]
#
# If no arguments provided, checks all grammars.
# If two git refs provided, only checks grammars that changed between them.

set -uo pipefail

# Get the directory where this script is located
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

# Get the repo root
PREFIX="$(cd "$SCRIPT_DIR/.." && pwd)"

# Find the stale links checker script
STALE_LINKS_SCRIPT="$SCRIPT_DIR/find-stale-links.sh"

if [[ ! -x "$STALE_LINKS_SCRIPT" ]]; then
    echo "Error: find-stale-links.sh not found or not executable at $STALE_LINKS_SCRIPT"
    exit 1
fi

grammars=()

if [[ $# -eq 0 ]]; then
    # No arguments - find all grammars with desc.xml
    echo "No git refs provided, checking all grammars..."
    pushd "$PREFIX" > /dev/null
    while IFS= read -r desc_file; do
        g=$(dirname "$desc_file")
        # Remove leading ./ if present
        g="${g#./}"
        grammars+=("$g")
    done < <(find . -name desc.xml -not -path '*/.git/*' | sort -u)
    popd > /dev/null
elif [[ $# -eq 2 ]]; then
    # Two arguments - find grammars that changed between the two refs
    BEFORE="$1"
    AFTER="$2"
    echo "Checking grammars changed between $BEFORE and $AFTER..."

    # Get list of changed files and extract their directories
    directories=$(git diff --name-only "$BEFORE" "$AFTER" -- . 2>/dev/null | \
        sed 's#\(.*\)[/][^/]*$#\1#' | \
        sort -u | \
        grep -v '_scripts' | \
        grep -v '.github' | \
        grep -v '^[.]$' || true)

    for g in $directories; do
        # Skip if directory doesn't exist (file was deleted)
        if [[ ! -d "$PREFIX/$g" ]]; then
            continue
        fi

        # Walk up to find the grammar root (directory with desc.xml)
        pushd "$PREFIX/$g" > /dev/null 2>&1 || continue
        while true; do
            if [[ -f desc.xml ]]; then
                break
            elif [[ "$(pwd)" == "$PREFIX" ]]; then
                break
            elif [[ "$(pwd)" == "/" ]]; then
                break
            fi
            cd ..
        done

        current=$(pwd)
        # Extract path relative to prefix
        g="${current##*$PREFIX}"
        g="${g##/}"

        popd > /dev/null 2>&1

        # Skip if we ended up at the root or no desc.xml found
        if [[ -z "$g" || "$g" == "." ]]; then
            continue
        fi
        if [[ ! -f "$PREFIX/$g/desc.xml" ]]; then
            continue
        fi

        grammars+=("$g")
    done

    # Remove duplicates
    if [[ ${#grammars[@]} -gt 0 ]]; then
        readarray -t grammars < <(printf '%s\n' "${grammars[@]}" | sort -u)
    fi
else
    echo "Usage: $0 [BEFORE_SHA AFTER_SHA]"
    exit 1
fi

echo "Found ${#grammars[@]} grammar(s) to check"

if [[ ${#grammars[@]} -eq 0 ]]; then
    echo "No grammars to check."
    exit 0
fi

# Check each grammar for stale links
failed=0
for grammar in "${grammars[@]}"; do
    echo ""
    echo "=== Checking $grammar ==="
    grammar_path="$PREFIX/$grammar"

    if "$STALE_LINKS_SCRIPT" --timeout 15 "$grammar_path"; then
        echo "OK: No stale links in $grammar"
    else
        echo "::warning file=$grammar,line=0,col=0,endColumn=0::Stale links detected in $grammar"
        failed=1
    fi
done

echo ""
if [[ $failed -eq 0 ]]; then
    echo "All grammars passed stale link check."
    exit 0
else
    echo "Some grammars have stale links."
    exit 1
fi
