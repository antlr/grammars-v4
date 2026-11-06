#!/usr/bin/env python3

import argparse
import hashlib
from pathlib import Path
from urllib.error import URLError
from urllib.request import urlopen


UNICODE_VERSION = "12.1.0"
UNICODE_DATA_URL = f"https://www.unicode.org/Public/{UNICODE_VERSION}/ucd/UnicodeData.txt"
UNICODE_DATA_SHA256 = "93ab1acd8fd9d450463b50ae77eab151a7cda48f98b25b56baed8070f80fc936"
OUTPUT_PATH = Path(__file__).resolve().parents[1] / "UnicodeClasses.g4"
CATEGORIES = (
    ("UNICODE_CLASS_LL", "Ll"),
    ("UNICODE_CLASS_LM", "Lm"),
    ("UNICODE_CLASS_LO", "Lo"),
    ("UNICODE_CLASS_LT", "Lt"),
    ("UNICODE_CLASS_LU", "Lu"),
    ("UNICODE_CLASS_ND", "Nd"),
    ("UNICODE_CLASS_NL", "Nl"),
)


def load_unicode_data(path):
    if path is None:
        try:
            with urlopen(UNICODE_DATA_URL, timeout=30) as response:
                data = response.read()
        except (OSError, URLError) as error:
            raise RuntimeError(f"cannot download {UNICODE_DATA_URL}: {error}") from error
    else:
        data = path.read_bytes()

    actual_sha256 = hashlib.sha256(data).hexdigest()
    if actual_sha256 != UNICODE_DATA_SHA256:
        raise RuntimeError(
            f"UnicodeData.txt SHA-256 mismatch: expected {UNICODE_DATA_SHA256}, got {actual_sha256}"
        )
    return data.decode("ascii")


def category_ranges(unicode_data):
    selected_categories = {category for _, category in CATEGORIES}
    code_points = {category: [] for category in selected_categories}
    range_start = None
    range_category = None

    for line_number, line in enumerate(unicode_data.splitlines(), start=1):
        fields = line.split(";")
        if len(fields) != 15:
            raise RuntimeError(f"UnicodeData.txt line {line_number} has {len(fields)} fields, expected 15")
        code_point = int(fields[0], 16)
        name = fields[1]
        category = fields[2]

        if name.endswith(", First>"):
            if range_start is not None:
                raise RuntimeError(f"nested UnicodeData range at line {line_number}")
            range_start = code_point
            range_category = category
        elif name.endswith(", Last>"):
            if range_start is None or range_category != category or code_point < range_start:
                raise RuntimeError(f"unmatched UnicodeData range end at line {line_number}")
            if category in selected_categories:
                code_points[category].extend(range(range_start, code_point + 1))
            range_start = None
            range_category = None
        elif category in selected_categories:
            if range_start is not None:
                raise RuntimeError(f"unfinished UnicodeData range at line {line_number}")
            code_points[category].append(code_point)

    if range_start is not None:
        raise RuntimeError("UnicodeData.txt ended inside a First/Last range")
    return {category: compress(points) for category, points in code_points.items()}


def compress(code_points):
    ranges = []
    for code_point in sorted(code_points):
        if ranges and code_point == ranges[-1][1] + 1:
            ranges[-1] = (ranges[-1][0], code_point)
        else:
            ranges.append((code_point, code_point))
    return ranges


def format_code_point(code_point):
    if code_point <= 0xFFFF:
        return f"'\\u{code_point:04X}'"
    return f"'\\u{{{code_point:X}}}'"


def render(ranges_by_category):
    lines = [
        "/*",
        f" * Generated from Unicode {UNICODE_VERSION} UnicodeData.txt.",
        f" * Source: {UNICODE_DATA_URL}",
        f" * SHA-256: {UNICODE_DATA_SHA256}",
        " */",
        "",
        "lexer grammar UnicodeClasses;",
        "",
    ]
    for rule_name, category in CATEGORIES:
        ranges = ranges_by_category[category]
        lines.append(f"{rule_name}:")
        for index, (start, end) in enumerate(ranges):
            expression = format_code_point(start)
            if end != start:
                expression += f" .. {format_code_point(end)}"
            prefix = "    " if index == 0 else "    | "
            lines.append(prefix + expression)
        lines.extend((";", ""))
    return "\n".join(lines)


def main():
    parser = argparse.ArgumentParser(description="Generate frozen Kotlin identifier categories from UnicodeData.txt")
    parser.add_argument("--unicode-data", type=Path, help="use a local UnicodeData.txt after verifying its checksum")
    parser.add_argument("--check", action="store_true", help="fail if UnicodeClasses.g4 is not up to date")
    arguments = parser.parse_args()

    try:
        expected = render(category_ranges(load_unicode_data(arguments.unicode_data)))
    except (OSError, RuntimeError) as error:
        parser.exit(1, f"error: {error}\n")

    if arguments.check:
        actual = OUTPUT_PATH.read_text(encoding="utf-8")
        if actual != expected:
            parser.exit(1, "UnicodeClasses.g4 is out of date; run tools/generate_unicode_classes.py\n")
        print("UnicodeClasses.g4 matches Unicode 12.1.0 UnicodeData.txt.")
        return

    OUTPUT_PATH.write_text(expected, encoding="utf-8", newline="\n")
    print(f"Wrote {OUTPUT_PATH.relative_to(Path.cwd())}")


if __name__ == "__main__":
    main()