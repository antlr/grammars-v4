"""Transforms the CSharp grammar files for the Dart target."""
import shutil
from glob import glob
from pathlib import Path
import sys


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
    with open(file_path + ".bak", 'r', encoding="utf-8") as input_file:
        with open(file_path, 'w', encoding="utf-8") as output_file:
            for line in input_file:
                output_file.write(line)
            output_file.flush()

    print("Writing ...")


if __name__ == '__main__':
    main()
