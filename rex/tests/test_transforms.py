"""Run from the repository root: python -m unittest discover -s rex/tests -v."""
from pathlib import Path
import subprocess
import sys
import tempfile
import unittest

REX = Path(__file__).resolve().parents[1]


class RustTransformTests(unittest.TestCase):
    def check_transform(self, text, expected_option=None):
        with tempfile.TemporaryDirectory() as directory:
            grammar = Path(directory) / "rexLexer.g4"
            grammar.write_text(text, encoding="utf-8")
            command = [sys.executable, str(REX / "Rust" / "transformGrammar.py")]
            subprocess.run(command, cwd=directory, check=True)
            first = grammar.read_bytes()
            result = first.decode("utf-8")
            self.assertNotIn("superClass", result)
            self.assertNotIn("this.Check1()", result)
            self.assertIn("recog.input.as_mut().unwrap().la(1)", result)
            if expected_option:
                self.assertIn(expected_option, result)
            else:
                self.assertNotIn("options", result)
            subprocess.run(command, cwd=directory, check=True)
            self.assertEqual(first, grammar.read_bytes())

    def test_formatted_source(self):
        self.check_transform((REX / "rexLexer.g4").read_text(encoding="utf-8"))

    def test_spacing_and_other_options(self):
        for block in (
            "options { superClass = LexerBase; }",
            "options\r\n{\r\n\tsuperClass=LexerBase ;\r\n}",
            "options { caseInsensitive = true; superClass = LexerBase; }",
        ):
            with self.subTest(block=block):
                self.check_transform(
                    "lexer grammar rexLexer;\n" + block + "\nA: 'a' { this.Check1() }?;",
                    "caseInsensitive = true;" if "caseInsensitive" in block else None,
                )


if __name__ == "__main__":
    unittest.main()
