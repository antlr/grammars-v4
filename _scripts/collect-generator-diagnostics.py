#!/usr/bin/env python3

"""Normalize generator diagnostics emitted by _scripts/test.sh."""

from __future__ import annotations

import argparse
import hashlib
import json
import re
import sys
import xml.etree.ElementTree as ElementTree
from collections import Counter, defaultdict
from pathlib import Path
from typing import Any


PREFIX_PATTERN = re.compile(r"^\[([^,]+),([^]]+)] (.*)$")
DIAGNOSTIC_PATTERN = re.compile(
    r"^(warning|error)\(([^)]+)\):\s*"
    r"(?:(.+?):(\d+):(\d+):\s*)?(.*)$"
)
MARKER_PATTERN = re.compile(
    r"^generator-inventory: status=([^ ]+)(?: stage=([^ ]+))?$"
)
ANSI_PATTERN = re.compile(r"\x1b\[[0-9;]*m")


def parse_arguments() -> argparse.Namespace:
    parser = argparse.ArgumentParser(
        description="Normalize ANTLR generator diagnostics and validate matrix coverage."
    )
    parser.add_argument("--log", required=True, type=Path)
    parser.add_argument("--output", required=True, type=Path)
    parser.add_argument("--repository", default=Path.cwd(), type=Path)
    parser.add_argument("--repository-commit", default="")
    parser.add_argument("--antlr-version", required=True)
    parser.add_argument("--antlr-jar-sha256", default="")
    parser.add_argument("--antlr-ng-commit", required=True)
    parser.add_argument("--antlr-ng-version", required=True)
    parser.add_argument("--test-exit-code", required=True, type=int)
    parser.add_argument("--targets", nargs="+", required=True)
    return parser.parse_args()


def target_is_enabled(target_expression: str, target: str) -> bool:
    enabled = False
    for item in re.split(r"[,;\s]+", target_expression.strip()):
        if item == "+all":
            enabled = True
        elif item == f"-{target}":
            enabled = False
        elif item == target:
            enabled = True
    return enabled


def discover_matrix(
    repository: Path, targets: list[str]
) -> tuple[list[dict[str, Any]], list[str]]:
    matrix: list[dict[str, Any]] = []
    errors: list[str] = []
    for descriptor in sorted(repository.rglob("desc.xml")):
        if any(part.startswith("Generated") for part in descriptor.parts):
            continue
        grammar = descriptor.parent.relative_to(repository).as_posix()
        try:
            root = ElementTree.parse(descriptor).getroot()
        except ElementTree.ParseError as error:
            errors.append(f"{descriptor.relative_to(repository)}: {error}")
            continue
        target_expression = root.findtext("targets", default="")
        antlr_constraint = root.findtext("antlr-version", default="")
        for target in targets:
            matrix.append(
                {
                    "antlrVersionConstraint": antlr_constraint.strip(),
                    "grammar": grammar,
                    "supported": target_is_enabled(target_expression, target),
                    "target": target,
                }
            )
    return matrix, errors


def normalize_source(repository: Path, grammar: str, emitted_source: str) -> str:
    source = emitted_source.replace("\\", "/")
    grammar_directory = repository / grammar
    candidates = list(grammar_directory.rglob(Path(source).name))
    if len(candidates) == 1:
        return candidates[0].relative_to(repository).as_posix()
    if not Path(source).is_absolute():
        return f"{grammar}/{source}"
    return source


def fingerprint(diagnostic: dict[str, Any]) -> str:
    stable_fields = (
        diagnostic["generator"],
        diagnostic["target"],
        diagnostic["grammar"],
        diagnostic["file"],
        diagnostic["severity"],
        diagnostic["code"],
        diagnostic["message"],
    )
    return hashlib.sha256("\0".join(map(str, stable_fields)).encode()).hexdigest()


def write_json(path: Path, value: Any) -> None:
    path.write_text(f"{json.dumps(value, indent=2, sort_keys=True)}\n")


def main() -> int:
    arguments = parse_arguments()
    repository = arguments.repository.resolve()
    output = arguments.output.resolve()
    output.mkdir(parents=True, exist_ok=True)

    expected_matrix, descriptor_errors = discover_matrix(repository, arguments.targets)
    statuses: dict[tuple[str, str], dict[str, str]] = {}
    pair_logs: dict[tuple[str, str], list[str]] = defaultdict(list)
    diagnostics: list[dict[str, Any]] = []
    unparsed_diagnostics: list[str] = []

    for raw_line in arguments.log.read_text(errors="replace").splitlines():
        line = ANSI_PATTERN.sub("", raw_line)
        prefix_match = PREFIX_PATTERN.match(line)
        if prefix_match is None:
            continue
        grammar, target, body = prefix_match.groups()
        pair_logs[(grammar, target)].append(body)

        marker_match = MARKER_PATTERN.match(body)
        if marker_match is not None:
            status, stage = marker_match.groups()
            statuses[(grammar, target)] = {
                "status": status,
                **({"stage": stage} if stage is not None else {}),
            }
            continue

        diagnostic_match = DIAGNOSTIC_PATTERN.match(body)
        if diagnostic_match is None:
            if re.search(r"\b(?:warning|error)\([^)]+\):", body):
                unparsed_diagnostics.append(line)
            continue

        severity, code, emitted_source, line_number, column, message = (
            diagnostic_match.groups()
        )
        generator = "antlr-ng" if target == "Antlr4ng" else "antlr"
        version = (
            arguments.antlr_ng_version
            if generator == "antlr-ng"
            else arguments.antlr_version
        )
        source = (
            normalize_source(repository, grammar, emitted_source)
            if emitted_source is not None
            else ""
        )
        diagnostic: dict[str, Any] = {
            "code": code,
            "column": int(column) if column is not None else None,
            "file": source,
            "generator": generator,
            "generatorVersion": version,
            "grammar": grammar,
            "line": int(line_number) if line_number is not None else None,
            "message": message,
            "severity": severity,
            "target": target,
        }
        if generator == "antlr-ng":
            diagnostic["generatorCommit"] = arguments.antlr_ng_commit
        diagnostic["fingerprint"] = fingerprint(diagnostic)
        diagnostics.append(diagnostic)

    raw_logs_directory = output / "raw-logs"
    for (grammar, target), lines in sorted(pair_logs.items()):
        path = raw_logs_directory / target / f"{grammar}.log"
        path.parent.mkdir(parents=True, exist_ok=True)
        path.write_text("\n".join(lines) + "\n")

    missing_pairs: list[str] = []
    unexpected_statuses: list[str] = []
    failed_pairs: list[str] = []
    for item in expected_matrix:
        key = (item["grammar"], item["target"])
        observed = statuses.get(key)
        if observed is None:
            item["status"] = "missing"
            missing_pairs.append(f"{key[0]},{key[1]}")
            continue
        item.update(observed)
        status = observed["status"]
        if status == "failed":
            failed_pairs.append(f"{key[0]},{key[1]}")
        if item["supported"] and status == "skipped":
            unexpected_statuses.append(f"{key[0]},{key[1]}: supported but skipped")
        if not item["supported"] and status != "skipped":
            unexpected_statuses.append(
                f"{key[0]},{key[1]}: unsupported but {status}"
            )

    diagnostics = list(
        {diagnostic["fingerprint"]: diagnostic for diagnostic in diagnostics}.values()
    )
    diagnostics.sort(
        key=lambda item: (
            item["grammar"],
            item["target"],
            item["file"],
            item["line"] or 0,
            item["column"] or 0,
            item["code"],
        )
    )
    with (output / "diagnostics.jsonl").open("w") as diagnostics_file:
        for diagnostic in diagnostics:
            diagnostics_file.write(f"{json.dumps(diagnostic, sort_keys=True)}\n")

    write_json(output / "matrix.json", expected_matrix)
    manifest = {
        "generators": {
            "antlr": {
                "jarSha256": arguments.antlr_jar_sha256,
                "version": arguments.antlr_version,
            },
            "antlr-ng": {
                "commit": arguments.antlr_ng_commit,
                "version": arguments.antlr_ng_version,
            },
        },
        "repositoryCommit": arguments.repository_commit,
        "targets": arguments.targets,
        "testExitCode": arguments.test_exit_code,
    }
    write_json(output / "manifest.json", manifest)

    status_counts = Counter(item["status"] for item in expected_matrix)
    warning_counts = Counter(
        (item["generator"], item["code"])
        for item in diagnostics
        if item["severity"] == "warning"
    )
    summary = [
        "# Generator warning inventory",
        "",
        "## Versions",
        "",
        f"- Official ANTLR: `{arguments.antlr_version}`",
        (
            f"- `antlr-ng`: `{arguments.antlr_ng_version}` "
            f"(`{arguments.antlr_ng_commit}`)"
        ),
        "",
        "## Matrix",
        "",
        "| Status | Count |",
        "| --- | ---: |",
    ]
    for status, count in sorted(status_counts.items()):
        summary.append(f"| {status} | {count} |")
    summary.extend(
        [
            "",
            "## Warnings",
            "",
            "| Generator | Code | Count |",
            "| --- | ---: | ---: |",
        ]
    )
    if warning_counts:
        for (generator, code), count in sorted(warning_counts.items()):
            summary.append(f"| {generator} | {code} | {count} |")
    else:
        summary.append("| — | — | 0 |")
    summary.extend(
        [
            "",
            f"Total diagnostics: **{len(diagnostics)}**.",
            "",
            "Warnings are inventory data and do not make this job fail.",
        ]
    )
    (output / "summary.md").write_text("\n".join(summary) + "\n")

    validation = {
        "descriptorErrors": descriptor_errors,
        "failedPairs": failed_pairs,
        "missingPairs": missing_pairs,
        "testExitCode": arguments.test_exit_code,
        "unexpectedStatuses": unexpected_statuses,
        "unparsedDiagnostics": unparsed_diagnostics,
    }
    write_json(output / "validation.json", validation)

    has_inventory_error = any(
        (
            descriptor_errors,
            failed_pairs,
            missing_pairs,
            unexpected_statuses,
            unparsed_diagnostics,
        )
    ) or arguments.test_exit_code != 0
    if has_inventory_error:
        print(json.dumps(validation, indent=2), file=sys.stderr)
        return 1
    return 0


if __name__ == "__main__":
    sys.exit(main())
