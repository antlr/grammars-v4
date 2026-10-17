"""Add @header import blocks to the CSharp grammar files for the Antlr4ng target."""
import re
import shutil
from glob import glob
from pathlib import Path

def main():
    """Executes the script."""
    for file in glob("./*.g4"):
        transform_grammar(file)

def transform_grammar(file_path: str) -> None:
    print("Altering " + file_path)
    if not Path(file_path).is_file():
        print(f"Could not find file: {file_path}")
        sys.exit(1)

    shutil.move(file_path, file_path + ".bak")
    with open(file_path + ".bak",'r', encoding="utf-8") as input_file:
        with open(file_path, 'w', encoding="utf-8") as output_file:
            for line in input_file:
                line = re.sub(r"(\/\/ Insert here @header for lexer\.)",\
                    '@header {import { CSharpLexerBase } from "./CSharpLexerBase.js"}', line)
                line = re.sub(r"(\/\/ Insert here @header for parser\.)",\
                    '@header {import { CSharpParserBase } from "./CSharpParserBase.js"}', line)
                # antlr4ng generated parser uses 'localContext', not '_localctx'
                line = line.replace('_localctx', 'localContext')
                output_file.write(line)

if __name__ == '__main__':
    main()
