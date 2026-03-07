#!/usr/bin/env bash
# test-dotnet.sh (examples/) — convenience wrapper.
# Compiles and runs every CSharp7*.cs file in this directory using the
# installed .NET SDK (LangVersion=7.3).
#
# Usage:  bash test-dotnet.sh
#
# Delegates to ../test-dotnet.sh, passing this directory as the examples path.

exec "$(dirname "${BASH_SOURCE[0]}")/../test-dotnet.sh" \
    "$(dirname "${BASH_SOURCE[0]}")" "$@"
