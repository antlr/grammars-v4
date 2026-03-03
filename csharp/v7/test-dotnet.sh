#!/usr/bin/env bash
# test-dotnet.sh — Compile and run C# 7 example files using the installed .NET SDK.
#
# Usage:  bash test-dotnet.sh [examples-dir]
#
# Each CSharp7*.cs file in examples-dir is compiled into its own temporary project
# with LangVersion=7.3 and then executed.  The script reports PASS/FAIL per file
# and exits non-zero if any file fails to compile or run.

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
EXAMPLES_DIR="${1:-$SCRIPT_DIR/examples}"

# ── pre-flight ───────────────────────────────────────────────────────────────

if ! command -v dotnet &>/dev/null; then
    echo "error: 'dotnet' not found in PATH" >&2
    exit 1
fi

if [ ! -d "$EXAMPLES_DIR" ]; then
    echo "error: examples directory not found: $EXAMPLES_DIR" >&2
    exit 1
fi

DOTNET_VERSION=$(dotnet --version)
MAJOR=$(echo "$DOTNET_VERSION" | cut -d. -f1)
TFM="net${MAJOR}.0"

echo "dotnet $DOTNET_VERSION  →  $TFM"
echo "examples: $EXAMPLES_DIR"
echo

# ── temp workspace ───────────────────────────────────────────────────────────

WORK_DIR=$(mktemp -d)
trap 'rm -rf "$WORK_DIR"' EXIT

# ── run each example ─────────────────────────────────────────────────────────

pass=0
fail=0
failed_names=()

for cs_file in "$EXAMPLES_DIR"/CSharp7*.cs; do
    [ -f "$cs_file" ] || continue

    name=$(basename "$cs_file" .cs)
    proj_dir="$WORK_DIR/$name"
    mkdir -p "$proj_dir"

    cp "$cs_file" "$proj_dir/Program.cs"

    cat > "$proj_dir/$name.csproj" <<CSPROJ
<Project Sdk="Microsoft.NET.Sdk">
  <PropertyGroup>
    <OutputType>Exe</OutputType>
    <TargetFramework>$TFM</TargetFramework>
    <LangVersion>7.3</LangVersion>
    <AllowUnsafeBlocks>true</AllowUnsafeBlocks>
    <Nullable>disable</Nullable>
    <ImplicitUsings>disable</ImplicitUsings>
  </PropertyGroup>
</Project>
CSPROJ

    printf "  %-35s " "$name"

    if output=$(dotnet run --project "$proj_dir" 2>&1); then
        echo "PASS"
        pass=$((pass + 1))
    else
        echo "FAIL"
        fail=$((fail + 1))
        failed_names+=("$name")
        # Indent and show compiler/runtime output for diagnosis
        echo "$output" | sed 's/^/    | /'
    fi
done

# ── summary ──────────────────────────────────────────────────────────────────

echo
if [ $fail -eq 0 ]; then
    echo "All $pass test(s) passed."
else
    echo "$pass passed, $fail failed: ${failed_names[*]}"
    exit 1
fi
