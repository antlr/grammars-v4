"""Replace 'this.' with 'self.' in grammar action blocks for the Python3 target."""
import shutil
from glob import glob
from pathlib import Path


def transform(file_path: str) -> None:
    print(f"Transforming {file_path}")
    shutil.move(file_path, file_path + ".bak")
    with open(file_path + ".bak", "r", encoding="utf-8") as f:
        text = f.read()
    text = text.replace("this.", "self.")
    with open(file_path, "w", encoding="utf-8") as f:
        f.write(text)


for g4 in glob("./*.g4"):
    transform(g4)
