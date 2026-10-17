"""Usage: python tests/run-tests.py /path/to/antlr4-4.13.2-complete.jar

Requires Java/Javac on PATH; builds in a temporary directory.
No installed Trash or Python ANTLR runtime is needed.
"""
import os
from pathlib import Path
import subprocess
import sys
import tempfile

root = Path(__file__).resolve().parents[1]
jar = Path(sys.argv[1]).resolve()
with tempfile.TemporaryDirectory(prefix="ixml-regressions-") as directory:
    build = Path(directory)
    subprocess.run([
        "java", "-jar", str(jar), "-Dlanguage=Java", "-encoding", "UTF-8",
        "-Xexact-output-dir", "-o", str(build),
        str(root / "ixmlLexer.g4"), str(root / "ixmlParser.g4"),
    ], check=True)
    subprocess.run([
        "javac", "-encoding", "UTF-8", "-cp", str(jar), "-d", str(build),
        *map(str, build.glob("*.java")), str(root / "tests" / "GrammarRegression.java"),
    ], check=True)
    # Same exclusion as desc.xml: this sample has non-iXML annotations.
    examples = sorted(p for p in (root / "examples").rglob("*.ixml")
                      if not p.name.endswith(".decorated.ixml"))
    subprocess.run([
        "java", "-cp", os.pathsep.join([str(build), str(jar)]),
        "GrammarRegression", *map(str, examples),
    ], check=True)
