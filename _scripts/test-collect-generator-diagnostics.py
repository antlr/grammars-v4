#!/usr/bin/env python3

import json
import subprocess
import sys
import tempfile
import unittest
from collections import Counter
from pathlib import Path


SCRIPT = Path(__file__).with_name("collect-generator-diagnostics.py")


class CollectGeneratorDiagnosticsTest(unittest.TestCase):
    def test_normalizes_plsql_warning_example_without_failing(self) -> None:
        with tempfile.TemporaryDirectory() as temporary_directory:
            root = Path(temporary_directory)
            repository = root / "repository"
            grammar = repository / "sql" / "plsql"
            grammar.mkdir(parents=True)
            (grammar / "desc.xml").write_text(
                "<desc><antlr-version>^4.12.0</antlr-version>"
                "<targets>Java;Antlr4ng</targets></desc>"
            )
            (grammar / "PlSqlParser.g4").write_text("parser grammar PlSqlParser;\n")
            log = root / "inventory.log"
            log.write_text(
                "[sql/plsql,Java] generator-inventory: status=started\n"
                "[sql/plsql,Java] warning(154): PlSqlParser.g4:1642:0: "
                "rule table_index_clause contains an optional block\n"
                "[sql/plsql,Java] warning(154): PlSqlParser.g4:3454:0: "
                "rule xmltype_table contains an optional block\n"
                "[sql/plsql,Java] warning(154): PlSqlParser.g4:3464:0: "
                "rule xmltype_column_properties contains an optional block\n"
                "[sql/plsql,Java] generator-inventory: status=completed\n"
                "[sql/plsql,Antlr4ng] generator-inventory: status=started\n"
                "[sql/plsql,Antlr4ng] warning(87): PlSqlParser.g4:1642:0: "
                "rule table_index_clause contains an optional block\n"
                "[sql/plsql,Antlr4ng] warning(87): PlSqlParser.g4:3454:0: "
                "rule xmltype_table contains an optional block\n"
                "[sql/plsql,Antlr4ng] warning(87): PlSqlParser.g4:3464:0: "
                "rule xmltype_column_properties contains an optional block\n"
                "[sql/plsql,Antlr4ng] generator-inventory: status=completed\n"
            )
            output = root / "output"

            result = subprocess.run(
                [
                    sys.executable,
                    str(SCRIPT),
                    "--log",
                    str(log),
                    "--output",
                    str(output),
                    "--repository",
                    str(repository),
                    "--antlr-version",
                    "4.13.2",
                    "--antlr-ng-commit",
                    "4975d4a7b556e34c38c2e53b9815c8c4c802236f",
                    "--antlr-ng-version",
                    "1.0.0",
                    "--test-exit-code",
                    "0",
                    "--targets",
                    "Java",
                    "Antlr4ng",
                ],
                check=False,
                capture_output=True,
                text=True,
            )

            self.assertEqual(result.returncode, 0, result.stderr)
            diagnostics = [
                json.loads(line)
                for line in (output / "diagnostics.jsonl").read_text().splitlines()
            ]
            self.assertEqual(
                Counter(
                    (item["generator"], item["code"]) for item in diagnostics
                ),
                Counter({("antlr-ng", "87"): 3, ("antlr", "154"): 3}),
            )
            self.assertEqual(
                diagnostics[0]["file"], "sql/plsql/PlSqlParser.g4"
            )
            self.assertEqual(
                diagnostics[0]["generatorCommit"],
                "4975d4a7b556e34c38c2e53b9815c8c4c802236f",
            )

    def test_fails_when_an_expected_pair_is_missing(self) -> None:
        with tempfile.TemporaryDirectory() as temporary_directory:
            root = Path(temporary_directory)
            repository = root / "repository"
            grammar = repository / "example"
            grammar.mkdir(parents=True)
            (grammar / "desc.xml").write_text(
                "<desc><targets>Java</targets></desc>"
            )
            log = root / "inventory.log"
            log.write_text("")

            result = subprocess.run(
                [
                    sys.executable,
                    str(SCRIPT),
                    "--log",
                    str(log),
                    "--output",
                    str(root / "output"),
                    "--repository",
                    str(repository),
                    "--antlr-version",
                    "4.13.2",
                    "--antlr-ng-commit",
                    "commit",
                    "--antlr-ng-version",
                    "version",
                    "--test-exit-code",
                    "0",
                    "--targets",
                    "Java",
                ],
                check=False,
                capture_output=True,
                text=True,
            )

            self.assertEqual(result.returncode, 1)
            validation = json.loads(
                (root / "output" / "validation.json").read_text()
            )
            self.assertEqual(validation["missingPairs"], ["example,Java"])


if __name__ == "__main__":
    unittest.main()
