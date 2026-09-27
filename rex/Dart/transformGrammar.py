"""Avoid the inherited Parser.context member in the Dart target."""
import re
from pathlib import Path

path = Path("rexParser.g4")
text = path.read_text(encoding="utf-8")
path.write_text(re.sub(r"\bcontext\b", "context_", text), encoding="utf-8")
