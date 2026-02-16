"""The script transforms the grammar to fit for the Go target."""
import sys
import os
import re
import shutil
from glob import glob
from pathlib import Path

def main(argv):
    for file in glob("./parser/*.g4"):
        fix(file)

def fix(file_path):
    print("Altering " + file_path)
    if not os.path.exists(file_path):
        print(f"Could not find file: {file_path}")
        sys.exit(1)

    shutil.move(file_path, file_path + ".bak")
    input_file = open(file_path + ".bak", 'r')
    content = input_file.read()
    input_file.close()

    is_parser = 'parser grammar' in content

    output_file = open(file_path, 'w')
    for x in content.splitlines(True):
        if 'this.' in x:
            if is_parser or '}?' in x:
                x = x.replace('this.', 'p.')
            else:
                x = x.replace('this.', 'l.')
        output_file.write(x)
        output_file.flush()

    print("Writing ...")
    output_file.close()

if __name__ == '__main__':
    main(sys.argv)
