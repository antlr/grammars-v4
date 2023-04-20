"""
MIT License

Copyright (c) 2022 Terence Parr

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
"""

import os
import sys
from pathlib import Path
import re
import json
from typing import Sequence

"""
Index all dirs in repo looking for dirs with complete info and dump a json object.
Example usage:

$ python mkindex.py "/Users/parrt/antlr/code/grammars-v4"

pom.xml files have stuff like:

<include>Java8Lexer.g4</include>
<include>Java8Parser.g4</include>

<entryPoint>compilationUnit</entryPoint>
<grammarName>Java8</grammarName>
<packageName></packageName>
<exampleFiles>examples/</exampleFiles>

just grab info from that then we can generate json:

{
		name: "tcpheader",
		lexer: "",
		parser: "https://raw.githubusercontent.com/antlr/grammars-v4/master/tcpheader/tcp.g4",
		start: "segmentheader",
		example: "https://raw.githubusercontent.com/antlr/grammars-v4/master/tcpheader/examples/example1.bin"
}
"""


def is_grammar_dir(path, files):
    "Return true if path has pom.xml and at least one .g4 file"
    pomfile = os.path.join(path, "pom.xml")
    if not os.path.exists(pomfile):
        return False
    for name in files:
        if name.endswith(".g4"):
            return True
    return False


def get_single_value(tagName, txt):
    values = get_values(tagName, txt)
    return values[0] if values is not None else None


def get_values(tagName, txt):
    values = re.findall(f"<{tagName}>(.*?)<", txt)
    if values is None or len(values) == 0:
        return None
    return values


def index_grammars(root : str) -> Sequence[dict]:
    entries = []
    for path, subdirs, files in os.walk(root):
        if is_grammar_dir(path, files):
            pomfile = os.path.join(path, "pom.xml")
            pom = Path(pomfile).read_text()

            grammarName = get_single_value("grammarName", pom)
            if grammarName is None:
                continue

            grammars = get_values("include", pom)
            if grammars is None:
                continue
            if len(grammars) > 1:
                lexer = grammars[0] if 'Lexer' in grammars[0] else grammars[1]
                parser = grammars[0] if 'Parser' in grammars[0] else grammars[1]
                gdir = path[len(root)+1:]
                if lexer:
                    lexer = f'https://raw.githubusercontent.com/antlr/grammars-v4/master/{gdir}/{lexer}'
                if parser:
                    parser = f'https://raw.githubusercontent.com/antlr/grammars-v4/master/{gdir}/{parser}'
            else:
                lexer = ""
                parser = grammars[0]
                gdir = path[len(root)+1:]
                if parser:
                    parser = f'https://raw.githubusercontent.com/antlr/grammars-v4/master/{gdir}/{parser}'

            exampleFilesDir = get_single_value("exampleFiles", pom)
            if exampleFilesDir is None:
                continue
            exampleFilesDir = os.path.join(path,exampleFilesDir)
            prefix = str(exampleFilesDir).replace('\\','/').removeprefix("./")
            if not os.path.exists(exampleFilesDir):
                continue
            examplesFiles = [str(f).replace('\\','/').removeprefix(prefix) for f in Path(exampleFilesDir).glob("**/*") if f.is_file() and not (f.parts[-1].endswith(".errors") or f.parts[-1].endswith(".tree")) ]

            entry = {
                'name' : grammarName,
                'lexer' : lexer,
                'parser' : parser,
                'start' : get_single_value("entryPoint", pom),
                'example' : examplesFiles
            }
            entries.append(entry)
    return entries


if __name__ == '__main__':
    if len(sys.argv) != 2:
        print("Usage: python mkindex.py grammars-v4-path")
        exit(1)
    grammars = index_grammars(sys.argv[1])
    print(json.dumps(grammars, indent=1))