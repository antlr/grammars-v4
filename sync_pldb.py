#!/usr/bin/env python3
"""
Synchronize PLDB and grammars-v4 cross-references.

For each grammar directory (identified by desc.xml):
- Ensure a readme exists with a ## Reference section and PLDB link (if matched)
- Ensure the PLDB .scroll file has an antlr entry pointing back

Usage:
    python sync_pldb.py                          # Apply changes
    python sync_pldb.py --dry-run                # Preview changes without writing
    python sync_pldb.py --grammars-v4 /path/to/grammars-v4 --pldb /path/to/pldb
"""

import argparse
import json
import os
import re
import glob as globmod

ANTLR_BASE_URL = "https://github.com/antlr/grammars-v4/tree/master"
PLDB_BASE_URL = "http://pldb.info/concepts"

# Regex to match any existing PLDB link in readmes
# Covers: pldb.io, pldb.info, pldb.pub, pldb.com, and archive.org wrapping any of those
PLDB_LINK_RE = re.compile(
    r'.*(?:pldb\.(?:io|info|pub|com)|archive\.org/[^\s)]*pldb\.(?:io|info|pub|com)).*',
    re.IGNORECASE,
)

# Regex to extract concept name from various PLDB URL formats
PLDB_CONCEPT_RE = re.compile(
    r'pldb\.(?:io|info|pub|com)/(?:concepts|rows)/([^./\s)]+?)(?:\.html)?(?:\s|\)|$)',
    re.IGNORECASE,
)

# ---------------------------------------------------------------------------
# Manual mapping table: grammar_dir -> pldb_concept_id
#
# This table supplements the automatic matching methods. Edit it to add
# entries that the heuristics cannot discover on their own.
# The mapping need not be one-to-one: multiple grammar dirs may point to the
# same PLDB concept.
# ---------------------------------------------------------------------------
MANUAL_MAPPING = {
    # --- Name mismatches discovered by inspection ---
    "arden":                        "arden-syntax",
    "asn/asn":                      "asn-1",
    "asn/asn_3gpp":                 "asn-1",
    "asm/asm8086":                  "x86-isa",
    "asm/asmMASM":                  "masm",
    "asm/asmRISCV":                 "risc-v",
    "asm/asmZ80":                   "z80",
    "asm/ptx/ptx-isa-1.0":         "ptx",
    "asm/ptx/ptx-isa-2.1":         "ptx",
    # "atl":                        "atlas-transformation-language",  # not in PLDB
    "capnproto":                    "capn-proto",
    "clf":                          "common-log-format",
    "clif":                         "common-logic",
    "cobol85":                      "cobol",
    "dart2":                        "dart",
    "flowmatic":                    "flow-matic",
    "fen":                          "fen-notation",
    # "gedcom":                     "gedcom",  # not in PLDB
    "gff3":                         "gff-format",
    # "kirikiri-tjs":               "tjs",  # not in PLDB
    # "langium":                    "langium",  # not in PLDB
    # "lark":                       "lark-parser",  # not in PLDB
    "lucene":                       "lucene-query-syntax",
    "modula2pim4":                  "modula-2",
    "morsecode":                    "morse-code",
    "protobuf/protobuf2":           "protobuf",
    "protobuf/protobuf3":           "protobuf",
    "smtlibv2":                     "smt",
    # "stringtemplate":             "stringtemplate",  # not in PLDB
    "tinybasic":                    "tiny-basic",
    "tinyos_nesc":                  "nesc",
    "wavefront":                    "wavefront-object",
    "wat":                          "wast",
    "xsd-regex":                    "xsd",
    # --- Versioned grammars -> parent concept ---
    "ada/ada83":                    "ada",
    "ada/ada2005":                  "ada",
    "ada/ada2012":                  "ada",
    "antlr/antlr2":                 "antlr",
    "antlr/antlr3":                 "antlr",
    "java/java8":                   "java",
    "java/java9":                   "java",
    "javascript/ecmascript":        "javascript",
    "kotlin/kotlin-formal":         "kotlin",
    "logo/ucb-logo":                "logo",
    "python/python2":               "python",
    "python/python2_7_18":          "python",
    "python/python2-js":            "python",
    "python/python3":               "python",
    "python/python3_13":            "python",
    "swift/swift2":                 "swift",
    "swift/swift3":                 "swift",
    "swift/swift5":                 "swift",
    "vba/vba6":                     "vba",
    "vba/vba_cc":                   "vba",
    "vba/vba_like":                 "vba",
    "vhdl/vhdl2008":                "vhdl",
    "xpath/xpath1":                 "xpath",
    "xpath/xpath20":                "xpath",
    "xpath/xpath31":                "xpath",
    # --- SQL variants -> specific PLDB concepts ---
    "sql/athena":                   "sql",
    # "sql/clickhouse":             "clickhouse",  # not in PLDB
    # "sql/cockroachdb":            "cockroachdb",  # not in PLDB
    "sql/databricks":               "sql",
    "sql/derby":                    "apache-derby",
    "sql/drill":                    "sql",
    "sql/hive/v2":                  "hiveql",
    "sql/hive/v3":                  "hiveql",
    "sql/hive/v4":                  "hiveql",
    "sql/informix-sql":             "informix",
    "sql/mysql/Positive-Technologies": "mysql",
    "sql/phoenix":                  "apache-phoenix",
    "sql/snowflake":                "sql",
    "sql/starrocks":                "sql",
    # "sql/trino":                  "trino",  # not in PLDB
    # --- Other ---
    "terraform":                    "hcl",
    # "rego":                       "rego",  # not in PLDB
    # "pddl":                       "pddl",  # not in PLDB
    "swift-fin":                    "swift",
    # "evm-bytecode":               "evm",  # not in PLDB
    "unicode/graphemes":            "unicode",
    "unicode/unicode16":            "unicode",
    "racket-bsl":                   "racket",
    "racket-isl":                   "racket",
}

# ---------------------------------------------------------------------------
# Parent directories that contain grammar subdirectories but have no desc.xml
# themselves. Map to the parent PLDB concept so a readme is created.
# ---------------------------------------------------------------------------
PARENT_DIR_MAPPING = {
    "ada":          "ada",
    "antlr":        "antlr",
    "asm":          None,           # no single PLDB concept
    "asn":          "asn-1",
    "esolang":      None,
    "fortran":      "fortran",
    "java":         "java",
    "javascript":   "javascript",
    "kotlin":       "kotlin",
    "logo":         "logo",
    "protobuf":     "protobuf",
    "python":       "python",
    "rfc822":       None,
    "sql":          "sql",
    "swift":        "swift",
    "unicode":      "unicode",
    "vba":          "vba",
    "verilog":      "verilog",
    "vhdl":         "vhdl",
    "xpath":        "xpath",
}


def parse_args():
    p = argparse.ArgumentParser(description=__doc__,
                                formatter_class=argparse.RawDescriptionHelpFormatter)
    p.add_argument("--grammars-v4",
                    default=os.path.normpath(os.path.dirname(os.path.abspath(__file__))),
                    help="Path to grammars-v4 repo (default: directory containing this script)")
    p.add_argument("--pldb",
                    default=os.path.normpath(r"C:\msys64\home\Kenne\pldb"),
                    help="Path to PLDB repo (default: C:\\msys64\\home\\Kenne\\pldb)")
    p.add_argument("--dry-run", action="store_true",
                    help="Preview changes without writing files")
    return p.parse_args()


# ---------------------------------------------------------------------------
# File I/O helpers that preserve original line endings
# ---------------------------------------------------------------------------

def read_file(path):
    """Read a file and return (text_with_normalized_newlines, eol_string)."""
    with open(path, "rb") as f:
        raw = f.read()
    eol = "\r\n" if b"\r\n" in raw else "\n"
    text = raw.decode("utf-8", errors="replace").replace("\r\n", "\n")
    return text, eol


def write_file(path, text, eol):
    """Write text to a file using the specified line ending."""
    content = text.replace("\n", eol)
    with open(path, "w", encoding="utf-8", newline="") as f:
        f.write(content)


# ---------------------------------------------------------------------------
# Phase 1: Build mapping
# ---------------------------------------------------------------------------

def find_grammar_dirs(grammars_dir):
    """Find all grammar directories (those containing desc.xml)."""
    dirs = []
    for root, _dirnames, filenames in os.walk(grammars_dir):
        if "desc.xml" in filenames:
            rel = os.path.relpath(root, grammars_dir).replace("\\", "/")
            if rel.startswith("_") or rel.startswith("."):
                continue
            dirs.append(rel)
    dirs.sort()
    return dirs


def find_parent_dirs(grammars_dir, grammar_dirs):
    """Find parent directories that contain grammar subdirs but have no desc.xml."""
    parents = set()
    for gdir in grammar_dirs:
        parts = gdir.split("/")
        if len(parts) >= 2:
            parents.add(parts[0])
    result = []
    for p in sorted(parents):
        abs_p = os.path.join(grammars_dir, p)
        if os.path.isdir(abs_p) and not os.path.isfile(os.path.join(abs_p, "desc.xml")):
            if p in PARENT_DIR_MAPPING:
                result.append(p)
    return result


def find_existing_readme(grammars_dir, grammar_dir):
    """Find existing readme file, preserving its case."""
    abs_dir = os.path.join(grammars_dir, grammar_dir)
    for name in os.listdir(abs_dir):
        if name.lower() == "readme.md":
            return os.path.join(abs_dir, name), name
    return None, None


def build_reverse_map_from_antlr_entries(concepts_dir):
    """Parse existing antlr entries in PLDB .scroll files."""
    mapping = {}
    scroll_files = globmod.glob(os.path.join(concepts_dir, "*.scroll"))
    for scroll_path in scroll_files:
        concept = os.path.splitext(os.path.basename(scroll_path))[0]
        text, _ = read_file(scroll_path)
        for line in text.split("\n"):
            if line.startswith("antlr "):
                url = line.strip().split(" ", 1)[1]
                if "antlr/grammars-v4" not in url:
                    continue
                m = re.search(r"grammars-v4/(?:tree|blob)/master/(.+?)(?:\s|$)", url)
                if m:
                    path = m.group(1).rstrip("/")
                    if path.endswith(".g4"):
                        path = os.path.dirname(path).replace("\\", "/")
                    mapping[path] = concept
                break
    return mapping


def extract_concept_from_readme(readme_path):
    """Extract PLDB concept name from existing readme links."""
    if readme_path is None:
        return None
    try:
        text, _ = read_file(readme_path)
    except (OSError, IOError):
        return None
    for m in PLDB_CONCEPT_RE.finditer(text):
        return m.group(1)
    return None


def build_concept_lookup(concepts_dir):
    """Build a case-insensitive lookup from lowercase name to actual concept id."""
    lookup = {}
    for fname in os.listdir(concepts_dir):
        if fname.endswith(".scroll"):
            concept = fname[:-len(".scroll")]
            lookup[concept.lower()] = concept
    return lookup


def build_pldb_name_lookup(pldb_dir):
    """Build a lookup from lowercase PLDB display name to concept id using pldb.json."""
    pldb_json = os.path.join(pldb_dir, "pldb.json")
    if not os.path.isfile(pldb_json):
        return {}
    data = json.load(open(pldb_json, encoding="utf-8"))
    return {e["name"].lower(): e["id"] for e in data if "name" in e and "id" in e}


def find_pldb_match(grammar_dir, concept_lookup, name_lookup):
    """Try to find a PLDB concept for a grammar using multiple heuristics."""
    basename = grammar_dir.split("/")[-1]
    bl = basename.lower()

    # 1. Direct concept id match (case-insensitive)
    if bl in concept_lookup:
        return concept_lookup[bl]

    # 2. Hyphenate at letter-digit boundaries: algol60 -> algol-60
    hyphenated = re.sub(r'([a-zA-Z])(\d)', r'\1-\2', basename)
    hyphenated = re.sub(r'(\d)([a-zA-Z])', r'\1-\2', hyphenated)
    hl = hyphenated.lower()
    if hl != bl and hl in concept_lookup:
        return concept_lookup[hl]

    # 3. PLDB display name match (exact)
    if bl in name_lookup:
        cid = name_lookup[bl]
        if cid.lower() in concept_lookup:
            return concept_lookup[cid.lower()]

    # 4. Hyphenated/spaced name match: algol60 -> "algol 60"
    spaced = re.sub(r'([a-zA-Z])(\d)', r'\1 \2', basename)
    spaced = re.sub(r'(\d)([a-zA-Z])', r'\1 \2', spaced)
    sl = spaced.lower()
    if sl != bl and sl in name_lookup:
        cid = name_lookup[sl]
        if cid.lower() in concept_lookup:
            return concept_lookup[cid.lower()]

    # 5. Replace underscores/hyphens with spaces for name lookup
    spaced2 = basename.replace("-", " ").replace("_", " ").lower()
    if spaced2 != bl and spaced2 in name_lookup:
        cid = name_lookup[spaced2]
        if cid.lower() in concept_lookup:
            return concept_lookup[cid.lower()]

    return None


def build_grammar_to_concept_mapping(grammar_dirs, grammars_dir, concepts_dir, pldb_dir):
    """Build the complete grammar_dir -> concept_name mapping."""
    mapping = {}
    unmatched = []

    concept_lookup = build_concept_lookup(concepts_dir)
    name_lookup = build_pldb_name_lookup(pldb_dir)
    reverse_map = build_reverse_map_from_antlr_entries(concepts_dir)

    for gdir in grammar_dirs:
        concept = None

        # Method 1: Manual mapping table (highest priority)
        if gdir in MANUAL_MAPPING:
            candidate = MANUAL_MAPPING[gdir]
            if candidate.lower() in concept_lookup:
                concept = concept_lookup[candidate.lower()]

        # Method 2: Reverse map from existing antlr entries
        if concept is None and gdir in reverse_map:
            concept = reverse_map[gdir]

        # Method 3: Existing PLDB links in readmes
        if concept is None:
            readme_path, _ = find_existing_readme(grammars_dir, gdir)
            concept = extract_concept_from_readme(readme_path)

        # Method 4: Direct name match + heuristics via pldb.json
        if concept is None:
            concept = find_pldb_match(gdir, concept_lookup, name_lookup)

        if concept:
            mapping[gdir] = concept
        else:
            unmatched.append(gdir)

    return mapping, unmatched


# ---------------------------------------------------------------------------
# Phase 2: Update readmes
# ---------------------------------------------------------------------------

def make_pldb_link_line(concept):
    return f"* [pldb]({PLDB_BASE_URL}/{concept})"


def update_readme(grammars_dir, grammar_dir, concept, dry_run=False):
    readme_path, readme_name = find_existing_readme(grammars_dir, grammar_dir)
    if readme_path is None:
        return _create_new_readme(grammars_dir, grammar_dir, concept, dry_run)
    else:
        return _update_existing_readme(readme_path, readme_name, grammar_dir, concept, dry_run)


def _create_new_readme(grammars_dir, grammar_dir, concept, dry_run):
    basename = grammar_dir.split("/")[-1]
    title = basename.replace("-", " ").replace("_", " ").title()

    lines = [f"# {title}", "", "## Reference", ""]
    if concept:
        lines.insert(3, make_pldb_link_line(concept))

    content = "\n".join(lines) + "\n"
    new_path = os.path.join(grammars_dir, grammar_dir, "readme.md")

    if not dry_run:
        with open(new_path, "w", encoding="utf-8", newline="\n") as f:
            f.write(content)

    return f"CREATE {grammar_dir}/readme.md" + (f" (pldb: {concept})" if concept else " (no pldb match)")


def _update_existing_readme(readme_path, readme_name, grammar_dir, concept, dry_run):
    text, eol = read_file(readme_path)
    lines = text.split("\n")
    changes = []

    lines, c = _rename_links_to_reference(lines); changes.extend(c)
    lines, c = _merge_duplicate_reference_sections(lines); changes.extend(c)
    lines, c = _handle_pldb_links(lines, concept); changes.extend(c)
    lines, c = _ensure_reference_section(lines, concept); changes.extend(c)

    new_text = "\n".join(lines)
    if new_text == text:
        return None

    if not dry_run:
        write_file(readme_path, new_text, eol)

    return f"UPDATE {grammar_dir}/{readme_name}: {'; '.join(changes)}"


def _is_links_heading(line):
    return line.strip() in ("## Links", "# Links")

def _is_reference_heading(line):
    return line.strip() == "## Reference"

def _is_any_heading(line):
    return line.strip().startswith("#")


def _rename_links_to_reference(lines):
    changes = []
    new_lines = []
    for line in lines:
        if _is_links_heading(line):
            new_lines.append("## Reference")
            changes.append("renamed Links -> Reference")
        else:
            new_lines.append(line)
    return new_lines, changes


def _merge_duplicate_reference_sections(lines):
    ref_positions = [i for i, line in enumerate(lines) if _is_reference_heading(line)]
    if len(ref_positions) <= 1:
        return lines, []
    first_ref = ref_positions[0]
    extra_content = []
    sections_to_remove = []
    for ref_pos in ref_positions[1:]:
        ref_end = _find_section_end(lines, ref_pos)
        for i in range(ref_pos + 1, ref_end):
            if lines[i].strip():
                extra_content.append(lines[i])
        sections_to_remove.append((ref_pos, ref_end))
    new_lines = list(lines)
    for start, end in reversed(sections_to_remove):
        del new_lines[start:end]
    if extra_content:
        new_first_end = _find_section_end(new_lines, first_ref)
        for i, cl in enumerate(extra_content):
            new_lines.insert(new_first_end + i, cl)
    return new_lines, ["merged duplicate Reference sections"]


def _find_section_end(lines, heading_idx):
    for i in range(heading_idx + 1, len(lines)):
        if _is_any_heading(lines[i]):
            return i
    return len(lines)


def _handle_pldb_links(lines, concept):
    changes = []
    if concept is None:
        return lines, changes

    pldb_line = make_pldb_link_line(concept)
    pldb_indices = [i for i, line in enumerate(lines) if PLDB_LINK_RE.match(line)]

    if pldb_indices:
        if len(pldb_indices) == 1 and lines[pldb_indices[0]].strip() == pldb_line:
            return lines, []
        new_lines = []
        replaced = False
        for i, line in enumerate(lines):
            if i in pldb_indices:
                if not replaced:
                    new_lines.append(pldb_line)
                    replaced = True
            else:
                new_lines.append(line)
        changes.append(f"updated PLDB link -> {concept}")
        return new_lines, changes

    # No existing PLDB link - add to Reference section if it exists
    for i, line in enumerate(lines):
        if _is_reference_heading(line):
            insert_idx = i + 1
            while insert_idx < len(lines) and lines[insert_idx].strip() == "":
                insert_idx += 1
            new_lines = list(lines)
            new_lines.insert(insert_idx, pldb_line)
            changes.append(f"added PLDB link ({concept})")
            return new_lines, changes

    return lines, changes


def _ensure_reference_section(lines, concept):
    for line in lines:
        if _is_reference_heading(line):
            return lines, []

    pldb_line = make_pldb_link_line(concept) if concept else None
    changes = []

    insert_idx = None
    for i, line in enumerate(lines):
        if line.strip().lower().startswith("## license"):
            insert_idx = i
            break

    new_section = ["## Reference", ""]
    if pldb_line:
        new_section.insert(1, pldb_line)
        changes.append(f"added Reference section with PLDB link ({concept})")
    else:
        changes.append("added Reference section")
    new_section.append("")

    new_lines = list(lines)
    if insert_idx is not None:
        if insert_idx > 0 and new_lines[insert_idx - 1].strip() != "":
            new_lines.insert(insert_idx, "")
            insert_idx += 1
        for i, sl in enumerate(new_section):
            new_lines.insert(insert_idx + i, sl)
    else:
        while new_lines and new_lines[-1].strip() == "":
            new_lines.pop()
        new_lines.append("")
        new_lines.extend(new_section)

    return new_lines, changes


# ---------------------------------------------------------------------------
# Phase 3: Update scroll files
# ---------------------------------------------------------------------------

def update_scroll_file(grammar_dir, concept, concepts_dir, dry_run=False):
    scroll_path = os.path.join(concepts_dir, concept + ".scroll")
    if not os.path.isfile(scroll_path):
        return None

    text, eol = read_file(scroll_path)
    correct_url = f"{ANTLR_BASE_URL}/{grammar_dir}"
    correct_line = f"antlr {correct_url}"
    lines = text.split("\n")

    antlr_idx = None
    for i, line in enumerate(lines):
        if line.startswith("antlr "):
            antlr_idx = i
            break

    if antlr_idx is not None:
        existing_url = lines[antlr_idx].strip().split(" ", 1)[1]
        if existing_url == correct_url:
            return None
        if "antlr/grammars-v4" in existing_url:
            lines[antlr_idx] = correct_line
            write_file(scroll_path, "\n".join(lines), eol) if not dry_run else None
            return f"FIX {concept}.scroll: antlr URL -> {correct_url}"
        else:
            return None
    else:
        insert_idx = _find_scroll_insert_position(lines)
        lines.insert(insert_idx, correct_line)
        write_file(scroll_path, "\n".join(lines), eol) if not dry_run else None
        return f"ADD {concept}.scroll: antlr {correct_url}"


def _find_scroll_insert_position(lines):
    last_kv_idx = 0
    for i, line in enumerate(lines):
        if i == 0:
            continue
        stripped = line.strip()
        if not stripped:
            continue
        if line.startswith(" ") or line.startswith("\t"):
            continue
        if stripped.startswith(("pygmentsHighlighter", "wikipedia ", "example",
                                "keywords ", "lineComment", "multiLine",
                                "has", "githubBigQuery", "semanticScholar",
                                "githubLanguage")):
            break
        if " " in stripped or stripped.startswith("isOpenSource") or stripped.startswith("fileType"):
            last_kv_idx = i
    return last_kv_idx + 1


def pick_primary_grammar_dir(grammar_dirs, concept, concepts_dir):
    scroll_path = os.path.join(concepts_dir, concept + ".scroll")
    if os.path.isfile(scroll_path):
        text, _ = read_file(scroll_path)
        for line in text.split("\n"):
            if line.startswith("antlr ") and "antlr/grammars-v4" in line:
                m = re.search(r"grammars-v4/(?:tree|blob)/master/(.+?)(?:\s|$)", line)
                if m:
                    existing = m.group(1).rstrip("/")
                    if existing.endswith(".g4"):
                        existing = os.path.dirname(existing).replace("\\", "/")
                    if existing in grammar_dirs:
                        return existing
                break
    return sorted(grammar_dirs, key=lambda d: (len(d), d))[0]


# ---------------------------------------------------------------------------
# Main
# ---------------------------------------------------------------------------

def main():
    args = parse_args()
    grammars_dir = os.path.normpath(args.grammars_v4)
    pldb_dir = os.path.normpath(args.pldb)
    concepts_dir = os.path.join(pldb_dir, "concepts")
    dry_run = args.dry_run

    if dry_run:
        print("=== DRY RUN MODE - No files will be modified ===\n")

    print(f"grammars-v4: {grammars_dir}")
    print(f"PLDB:        {pldb_dir}")
    print(f"concepts:    {concepts_dir}\n")

    # Phase 1
    print("Phase 1: Building grammar-to-PLDB mapping...")
    grammar_dirs = find_grammar_dirs(grammars_dir)
    print(f"  Found {len(grammar_dirs)} grammar directories")

    mapping, unmatched = build_grammar_to_concept_mapping(
        grammar_dirs, grammars_dir, concepts_dir, pldb_dir)
    print(f"  Matched: {len(mapping)} grammars")
    print(f"  Unmatched: {len(unmatched)} grammars")

    if unmatched:
        print("\n  Unmatched grammars:")
        for gdir in unmatched:
            print(f"    - {gdir}")

    concept_to_dirs = {}
    for gdir, concept in mapping.items():
        concept_to_dirs.setdefault(concept, []).append(gdir)

    primary_dirs = {}
    for concept, dirs in concept_to_dirs.items():
        primary_dirs[concept] = pick_primary_grammar_dir(dirs, concept, concepts_dir)

    # Phase 2
    print("\nPhase 2: Updating readmes in grammars-v4...")
    readme_changes = []
    for gdir in grammar_dirs:
        concept = mapping.get(gdir)
        result = update_readme(grammars_dir, gdir, concept, dry_run)
        if result:
            readme_changes.append(result)
            print(f"  {result}")

    # Phase 2b: parent directories
    concept_lookup = build_concept_lookup(concepts_dir)
    parent_dirs = find_parent_dirs(grammars_dir, grammar_dirs)
    for pdir in parent_dirs:
        concept = PARENT_DIR_MAPPING.get(pdir)
        if concept and concept.lower() not in concept_lookup:
            concept = None
        elif concept:
            concept = concept_lookup[concept.lower()]
        result = update_readme(grammars_dir, pdir, concept, dry_run)
        if result:
            readme_changes.append(result)
            print(f"  {result}")

    print(f"\n  Total readme changes: {len(readme_changes)}")

    # Phase 3
    print("\nPhase 3: Updating .scroll files in PLDB...")
    scroll_changes = []
    for concept, primary_gdir in sorted(primary_dirs.items()):
        result = update_scroll_file(primary_gdir, concept, concepts_dir, dry_run)
        if result:
            scroll_changes.append(result)
            print(f"  {result}")

    print(f"\n  Total scroll changes: {len(scroll_changes)}")

    print(f"\n{'='*60}")
    print(f"Summary:")
    print(f"  Grammars processed: {len(grammar_dirs)}")
    print(f"  PLDB matches: {len(mapping)}")
    print(f"  Unmatched: {len(unmatched)}")
    print(f"  Readme files changed: {len(readme_changes)}")
    print(f"  Scroll files changed: {len(scroll_changes)}")
    if dry_run:
        print("\n  (DRY RUN - no files were actually modified)")


if __name__ == "__main__":
    main()
