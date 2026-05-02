#!/usr/bin/env bash
# Build all grammars and combine their JARs into one fat JAR at
# target/all-grammars-1.0-SNAPSHOT.jar.
# Run from the repo root.
# Set SKIP_MAVEN_BUILD=1 to assemble from already-built module JARs.
set -euo pipefail

myrealpath() {
    f=$@
    if [ -d "$f" ]; then
        base=""
        dir="$f"
    else
        base="/$(basename "$f")"
        dir=$(dirname "$f")
    fi
    dir=$(cd "$dir" && /bin/pwd)
    echo "$dir$base"
}

if [[ "${SKIP_MAVEN_BUILD:-0}" == "1" ]]; then
    echo "Skipping Maven build; assembling from existing grammar JARs..."
else
    echo "Building all grammars..."
    mvn -B clean test package -Pgrammarv4
fi

TMPDIR=$(mktemp -d)
trap 'rm -rf "$TMPDIR"' EXIT

echo "Collecting compiled grammar JARs..."
while IFS= read -r jar; do
    abs=$(myrealpath "$jar")
    jar_tmp=$(mktemp -d)
    (cd "$jar_tmp" && jar xf "$abs" && rm -rf META-INF)
    cp -a "$jar_tmp"/. "$TMPDIR"/
    rm -rf "$jar_tmp"
done < <(find . -path "*/target/*.jar" \
    ! -name "*-sources.jar" \
    ! -name "*-javadoc.jar" \
    ! -name "*-tests.jar" \
    ! -name "*-test-sources.jar" \
    ! -path "./target/*" | LC_ALL=C sort)

mkdir -p target
OUTPUT="target/all-grammars-1.0-SNAPSHOT.jar"
echo "Assembling $OUTPUT ..."
jar cf "$OUTPUT" -C "$TMPDIR" .
echo "Done: $OUTPUT"
