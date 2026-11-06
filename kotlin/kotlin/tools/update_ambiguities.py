#!/usr/bin/env python3

import argparse
import os
import re
import subprocess
import tempfile
from pathlib import Path


GRAMMAR_DIR = Path(__file__).resolve().parents[1]
README_PATH = GRAMMAR_DIR / "README.md"
JAVA_PACKAGE = "org.antlr.grammars"
JAVA_CLASS = "AmbiguityAudit"
SECTION_TITLE = "### Ambiguities"

AUDIT_SOURCE = r'''package org.antlr.grammars;

import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;
import java.util.Set;
import java.util.TreeSet;
import org.antlr.v4.runtime.BaseErrorListener;
import org.antlr.v4.runtime.CharStreams;
import org.antlr.v4.runtime.CommonTokenStream;
import org.antlr.v4.runtime.Parser;
import org.antlr.v4.runtime.ParserRuleContext;
import org.antlr.v4.runtime.RecognitionException;
import org.antlr.v4.runtime.Recognizer;
import org.antlr.v4.runtime.Token;
import org.antlr.v4.runtime.atn.ATNConfigSet;
import org.antlr.v4.runtime.atn.PredictionMode;
import org.antlr.v4.runtime.dfa.DFA;

public final class AmbiguityAudit {
    private static final class AuditListener extends BaseErrorListener {
        private final List<String> diagnostics = new ArrayList<>();
        private final Set<String> ambiguities = new TreeSet<>();

        @Override
        public void syntaxError(Recognizer<?, ?> recognizer, Object offendingSymbol, int line, int column,
                String message, RecognitionException exception) {
            diagnostics.add("line " + line + ":" + column + " " + message);
        }

        @Override
        public void reportAmbiguity(Parser parser, DFA dfa, int startIndex, int stopIndex, boolean exact,
                java.util.BitSet ambiguousAlternatives, ATNConfigSet configs) {
            if (exact) {
                int ruleIndex = parser.getATN().decisionToState.get(dfa.decision).ruleIndex;
                ambiguities.add(parser.getRuleNames()[ruleIndex]);
            }
        }
    }

    public static void main(String[] args) throws Exception {
        Set<String> allAmbiguities = new TreeSet<>();
        int fixtureCount = 0;
        for (String argument : args) {
            Path path = Path.of(argument);
            boolean script = path.toString().endsWith(".kts");
            AuditListener listener = new AuditListener();
            KotlinLexer lexer = new KotlinLexer(CharStreams.fromString(Files.readString(path)));
            lexer.removeErrorListeners();
            lexer.addErrorListener(listener);
            CommonTokenStream tokens = new CommonTokenStream(lexer);
            tokens.fill();
            tokens.seek(0);

            KotlinParser parser = new KotlinParser(tokens);
            parser.removeErrorListeners();
            parser.addErrorListener(listener);
            parser.getInterpreter().setPredictionMode(PredictionMode.LL_EXACT_AMBIG_DETECTION);
            ParserRuleContext tree = script ? parser.script() : parser.kotlinFile();
            if (parser.getCurrentToken().getType() != Token.EOF) {
                listener.diagnostics.add("input was not completely consumed at " + parser.getCurrentToken().getText());
            }
            if (!listener.diagnostics.isEmpty()) {
                throw new IllegalStateException(path + ": " + String.join("; ", listener.diagnostics));
            }
            if (tree == null) {
                throw new IllegalStateException(path + ": parser returned no parse tree");
            }
            allAmbiguities.addAll(listener.ambiguities);
            fixtureCount++;
        }
        if (fixtureCount == 0) {
            throw new IllegalStateException("No positive Kotlin example fixtures were found");
        }
        for (String rule : allAmbiguities) {
            System.out.println("AMBIGUITY:" + rule);
        }
        System.out.println("FIXTURES:" + fixtureCount);
    }
}
'''


class UpdateError(Exception):
    pass


def positive_fixtures(grammar_dir=GRAMMAR_DIR):
    fixtures = []
    for subdirectory, suffix in (("examples/kotlinFile", ".kt"), ("examples/script", ".kts")):
        directory = Path(grammar_dir) / subdirectory
        if not directory.is_dir():
            raise UpdateError(f"missing example directory: {directory}")
        for path in directory.rglob(f"*{suffix}"):
            if not Path(str(path) + ".errors").is_file():
                fixtures.append(path)
    return sorted(fixtures, key=lambda path: path.as_posix())


def replace_ambiguities_section(readme, rules):
    lines = readme.splitlines(keepends=True)
    headings = [index for index, line in enumerate(lines) if line.rstrip("\r\n") == SECTION_TITLE]
    if len(headings) != 1:
        raise UpdateError(f"expected exactly one {SECTION_TITLE} heading, found {len(headings)}")

    start = headings[0]
    end = len(lines)
    for index in range(start + 1, len(lines)):
        if re.match(r"^#{1,3}\s", lines[index]):
            end = index
            break

    normalized_rules = sorted(set(rules))
    if not normalized_rules:
        raise UpdateError("exact ambiguity audit returned no parser rules")
    body = [
        "\n",
        "Generated by `python3 tools/update_ambiguities.py --antlr-jar PATH` using exact ambiguity detection "
        "(`LL_EXACT_AMBIG_DETECTION`) over successfully parsed `.kt` and `.kts` fixtures without adjacent `.errors` "
        "baselines. The example corpus is not exhaustive.\n",
        "Run the command above to refresh this list, or add `--check` to verify it without writing.\n",
        "\n",
        "```text\n",
        *(f"{rule}\n" for rule in normalized_rules),
        "```\n",
    ]
    replacement = lines[: start + 1] + body
    if end < len(lines):
        replacement.extend(lines[end:])
    return "".join(replacement)


def run_audit(antlr_jar, grammar_dir=GRAMMAR_DIR):
    antlr_jar = Path(antlr_jar).resolve()
    if not antlr_jar.is_file():
        raise UpdateError(f"ANTLR complete jar does not exist: {antlr_jar}")
    fixtures = positive_fixtures(grammar_dir)
    if not fixtures:
        raise UpdateError("No positive Kotlin example fixtures were found")

    target_dir = Path(grammar_dir) / "target"
    target_dir.mkdir(exist_ok=True)
    with tempfile.TemporaryDirectory(prefix="ambiguity-update-", dir=target_dir) as temporary:
        temporary_dir = Path(temporary)
        generated_dir = temporary_dir / "generated"
        classes_dir = temporary_dir / "classes"
        classes_dir.mkdir()
        source_file = temporary_dir / f"{JAVA_CLASS}.java"
        source_file.write_text(AUDIT_SOURCE, encoding="utf-8", newline="\n")

        run_command(
            [
                "java", "-jar", str(antlr_jar), "-Dlanguage=Java", "-package", JAVA_PACKAGE,
                "-no-listener", "-no-visitor", "-lib", str(grammar_dir), "-o", str(generated_dir),
                "KotlinLexer.g4", "KotlinParser.g4",
            ],
            cwd=grammar_dir,
        )
        generated_sources = [generated_dir / "KotlinLexer.java", generated_dir / "KotlinParser.java"]
        run_command(
            [
                "javac", "-cp", str(antlr_jar), "-d", str(classes_dir),
                *(str(path) for path in generated_sources), str(source_file),
            ],
            cwd=grammar_dir,
        )
        result = run_command(
            [
                "java", "-cp", f"{antlr_jar}{os.pathsep}{classes_dir}",
                f"{JAVA_PACKAGE}.{JAVA_CLASS}", *(str(path) for path in fixtures),
            ],
            cwd=grammar_dir,
        )

    rules = []
    reported_fixtures = None
    for line in result.stdout.splitlines():
        if line.startswith("AMBIGUITY:"):
            rules.append(line.removeprefix("AMBIGUITY:"))
        elif line.startswith("FIXTURES:"):
            reported_fixtures = int(line.removeprefix("FIXTURES:"))
        else:
            raise UpdateError(f"unexpected audit output: {line}")
    if reported_fixtures != len(fixtures):
        raise UpdateError(f"audited {reported_fixtures} fixtures, expected {len(fixtures)}")
    return sorted(set(rules)), len(fixtures)


def run_command(command, cwd):
    try:
        result = subprocess.run(command, cwd=cwd, text=True, capture_output=True, check=False)
    except OSError as error:
        raise UpdateError(f"cannot run {command[0]}: {error}") from error
    if result.returncode != 0:
        output = (result.stdout + result.stderr).strip()
        raise UpdateError(f"command failed ({result.returncode}): {' '.join(command)}\n{output}")
    return result


def update_readme(readme_path, rules, check=False):
    readme_path = Path(readme_path)
    original = readme_path.read_text(encoding="utf-8")
    updated = replace_ambiguities_section(original, rules)
    if check and original != updated:
        raise UpdateError("README.md Ambiguities section is out of date; run tools/update_ambiguities.py")
    if not check and original != updated:
        readme_path.write_text(updated, encoding="utf-8", newline="\n")
    return original != updated


def main(argv=None):
    parser = argparse.ArgumentParser(description="Refresh the Kotlin README exact-ambiguity list")
    parser.add_argument("--antlr-jar", required=True, type=Path, help="path to the ANTLR complete jar")
    parser.add_argument("--check", action="store_true", help="fail if README.md would change")
    arguments = parser.parse_args(argv)

    try:
        rules, fixture_count = run_audit(arguments.antlr_jar)
        changed = update_readme(README_PATH, rules, arguments.check)
    except (OSError, UpdateError, ValueError) as error:
        parser.exit(1, f"error: {error}\n")

    if arguments.check:
        print(f"README.md ambiguity list matches {len(rules)} exact rules from {fixture_count} fixtures.")
        return

    if changed:
        print(f"Updated README.md with {len(rules)} exact ambiguity rules from {fixture_count} fixtures.")
    else:
        print(f"README.md is current: {len(rules)} exact ambiguity rules from {fixture_count} fixtures.")


if __name__ == "__main__":
    main()