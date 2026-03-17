"""The script transforms the grammar to fit for the python target """
import sys
import re
import shutil
from glob import glob
from pathlib import Path

def main():
    """Executes the script."""
    for file in glob("./*.g4"):
        transform_grammar(file)

def transform_grammar(file_path):
    """Transforms the grammar to fit for the python target"""
    print("Altering " + file_path)
    if not Path(file_path).is_file():
        print(f"Could not find file: {file_path}")
        sys.exit(1)

    shutil.move(file_path, file_path + ".bak")
    with open(file_path + ".bak",'r', encoding="utf-8") as input_file:
        with open(file_path, 'w', encoding="utf-8") as output_file:
            for line in input_file:
                line = re.sub(r"(!this\.)", 'not self.', line)
                line = re.sub(r"(this\.)", 'self.', line)
                # Python LA() returns int; replace char literals in LookAhead calls
                line = re.sub(r"(LookAheadIs\w*\([^,]+,\s*)'}'", r'\g<1>125', line)
                line = re.sub(r"(LookAheadIs\w*\([^,]+,\s*)'{'", r'\g<1>123', line)
                # Python generated parser uses 'localctx', not '_localctx'
                line = line.replace('_localctx', 'localctx')
                # Mode constants need self. prefix in Python (class variable access)
                line = re.sub(r'\bPeekModeIs\(IRS_CONT\)', 'PeekModeIs(self.IRS_CONT)', line)
                line = re.sub(r'\bPeekModeIs\(IVS_CONT\)', 'PeekModeIs(self.IVS_CONT)', line)
                # Remove leading space after '{' in action blocks to avoid IndentationError
                line = re.sub(r'\{ (self\.)', r'{\1', line)
                output_file.write(line)

    print("Writing ...")

if __name__ == '__main__':
    main()
