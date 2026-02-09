#!/usr/bin/env python3
"""
Synchronize PLDB and grammars-v4 cross-references.

For each grammar directory (identified by desc.xml):
- Ensure a readme exists with a ## Reference section and PLDB link (if matched)
- Ensure the PLDB .scroll file has an antlr entry pointing back

Usage:
    python sync_pldb.py              # Apply changes
    python sync_pldb.py --dry-run    # Preview changes without writing
"""

import os
import re
import sys
import glob as globmod

GRAMMARS_DIR = os.path.normpath(os.path.dirname(os.path.abspath(__file__)))
PLDB_CONCEPTS_DIR = os.path.normpath(r"C:\msys64\home\Kenne\pldb\concepts")

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


def find_grammar_dirs():
    """Find all grammar directories (those containing desc.xml)."""
    dirs = []
    for root, _dirnames, filenames in os.walk(GRAMMARS_DIR):
        if "desc.xml" in filenames:
            rel = os.path.relpath(root, GRAMMARS_DIR).replace("\\", "/")
            # Skip _scripts and other dot/underscore dirs
            if rel.startswith("_") or rel.startswith("."):
                continue
            dirs.append(rel)
    dirs.sort()
    return dirs


def find_existing_readme(grammar_dir):
    """Find existing readme file, preserving its case. Returns (abs_path, filename) or (None, None)."""
    abs_dir = os.path.join(GRAMMARS_DIR, grammar_dir)
    for name in os.listdir(abs_dir):
        if name.lower() == "readme.md":
            return os.path.join(abs_dir, name), name
    return None, None


def build_reverse_map_from_antlr_entries():
    """Phase 1, Method 1: Parse existing antlr entries in PLDB .scroll files.
    Returns dict: grammar_relative_path -> concept_name
    """
    mapping = {}
    scroll_files = globmod.glob(os.path.join(PLDB_CONCEPTS_DIR, "*.scroll"))
    for scroll_path in scroll_files:
        concept = os.path.splitext(os.path.basename(scroll_path))[0]
        with open(scroll_path, "r", encoding="utf-8", errors="replace") as f:
            for line in f:
                if line.startswith("antlr "):
                    url = line.strip().split(" ", 1)[1]
                    # Only process grammars-v4 URLs
                    if "antlr/grammars-v4" not in url:
                        continue
                    # Extract the path after tree/master/ or blob/master/
                    m = re.search(r"grammars-v4/(?:tree|blob)/master/(.+?)(?:\s|$)", url)
                    if m:
                        path = m.group(1).rstrip("/")
                        # If path points to a specific file, use its directory
                        if path.endswith(".g4"):
                            path = os.path.dirname(path).replace("\\", "/")
                        mapping[path] = concept
                    break
    return mapping


def extract_concept_from_readme(readme_path):
    """Phase 1, Method 2: Extract PLDB concept name from existing readme links."""
    if readme_path is None:
        return None
    try:
        with open(readme_path, "r", encoding="utf-8", errors="replace") as f:
            content = f.read()
    except (OSError, IOError):
        return None

    for m in PLDB_CONCEPT_RE.finditer(content):
        return m.group(1)
    return None


def build_concept_lookup():
    """Build a case-insensitive lookup from lowercase name to actual concept name."""
    lookup = {}
    for fname in os.listdir(PLDB_CONCEPTS_DIR):
        if fname.endswith(".scroll"):
            concept = fname[:-len(".scroll")]
            lookup[concept.lower()] = concept
    return lookup


# Global concept lookup (populated once)
_concept_lookup = None


def get_concept_lookup():
    global _concept_lookup
    if _concept_lookup is None:
        _concept_lookup = build_concept_lookup()
    return _concept_lookup


def find_direct_match(grammar_dir):
    """Phase 1, Method 3: Check if {grammar_basename}.scroll exists in PLDB concepts."""
    basename = grammar_dir.split("/")[-1]  # e.g., "java8" from "java/java8"
    lookup = get_concept_lookup()
    return lookup.get(basename.lower())


def build_grammar_to_concept_mapping(grammar_dirs):
    """Build the complete grammar_dir -> concept_name mapping."""
    mapping = {}
    unmatched = []

    # Method 1: Reverse map from existing antlr entries
    reverse_map = build_reverse_map_from_antlr_entries()

    for gdir in grammar_dirs:
        concept = None

        # Method 1: Check reverse map
        if gdir in reverse_map:
            concept = reverse_map[gdir]
        else:
            # Method 2: Check readme for PLDB links
            readme_path, _ = find_existing_readme(gdir)
            concept = extract_concept_from_readme(readme_path)

            if concept is None:
                # Method 3: Direct name match
                concept = find_direct_match(gdir)

        if concept:
            mapping[gdir] = concept
        else:
            unmatched.append(gdir)

    return mapping, unmatched


def make_pldb_link_line(concept):
    """Create the standardized PLDB link line."""
    return f"* [pldb]({PLDB_BASE_URL}/{concept})"


def update_readme(grammar_dir, concept, dry_run=False):
    """Phase 2: Update or create readme for a grammar directory.

    Returns a description of what was done, or None if no change needed.
    """
    readme_path, readme_name = find_existing_readme(grammar_dir)

    if readme_path is None:
        # Create new readme
        return _create_new_readme(grammar_dir, concept, dry_run)
    else:
        # Update existing readme
        return _update_existing_readme(readme_path, readme_name, grammar_dir, concept, dry_run)


def _create_new_readme(grammar_dir, concept, dry_run):
    """Create a new readme.md for a grammar that has none."""
    # Use the last component as the grammar name, title-cased
    basename = grammar_dir.split("/")[-1]
    # Convert hyphenated/underscored names to title case
    title = basename.replace("-", " ").replace("_", " ").title()

    lines = [f"# {title}", "", "## Reference", ""]
    if concept:
        lines.insert(3, make_pldb_link_line(concept))

    content = "\n".join(lines) + "\n"
    new_path = os.path.join(GRAMMARS_DIR, grammar_dir, "readme.md")

    if not dry_run:
        with open(new_path, "w", encoding="utf-8", newline="\n") as f:
            f.write(content)

    return f"CREATE {grammar_dir}/readme.md" + (f" (pldb: {concept})" if concept else " (no pldb match)")


def _update_existing_readme(readme_path, readme_name, grammar_dir, concept, dry_run):
    """Update an existing readme: standardize sections, update PLDB links."""
    with open(readme_path, "r", encoding="utf-8", errors="replace") as f:
        original = f.read()

    lines = original.split("\n")
    changes = []

    # Step 1: Rename ## Links / # Links to ## Reference
    # But only rename "## Links" or "# Links" headings, not "## References and Links" etc.
    lines, rename_changes = _rename_links_to_reference(lines)
    changes.extend(rename_changes)

    # Step 2: If both ## Links and ## Reference existed (after rename there may be two ## Reference),
    # merge them
    lines, merge_changes = _merge_duplicate_reference_sections(lines)
    changes.extend(merge_changes)

    # Step 3: Handle PLDB links
    lines, pldb_changes = _handle_pldb_links(lines, concept)
    changes.extend(pldb_changes)

    # Step 4: Ensure ## Reference section exists
    lines, ref_changes = _ensure_reference_section(lines, concept)
    changes.extend(ref_changes)

    new_content = "\n".join(lines)

    if new_content == original:
        return None  # No changes

    if not dry_run:
        with open(readme_path, "w", encoding="utf-8", newline="\n") as f:
            f.write(new_content)

    change_desc = "; ".join(changes)
    return f"UPDATE {grammar_dir}/{readme_name}: {change_desc}"


def _is_links_heading(line):
    """Check if a line is a ## Links or # Links heading (exact match, not '## Links and References' etc.)."""
    stripped = line.strip()
    return stripped in ("## Links", "# Links")


def _is_reference_heading(line):
    """Check if a line is a ## Reference heading."""
    stripped = line.strip()
    return stripped == "## Reference"


def _is_any_heading(line):
    """Check if a line is any markdown heading."""
    return line.strip().startswith("#")


def _rename_links_to_reference(lines):
    """Rename '## Links' / '# Links' to '## Reference'."""
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
    """If there are multiple ## Reference sections, merge content into the first one."""
    # Find all ## Reference section positions
    ref_positions = []
    for i, line in enumerate(lines):
        if _is_reference_heading(line):
            ref_positions.append(i)

    if len(ref_positions) <= 1:
        return lines, []

    # Keep the first ## Reference section, merge others into it
    # Find the end of first reference section (next heading or EOF)
    first_ref = ref_positions[0]
    first_ref_end = _find_section_end(lines, first_ref)

    # Collect content from subsequent ## Reference sections
    extra_content = []
    # Process in reverse order so indices remain valid during removal
    sections_to_remove = []
    for ref_pos in ref_positions[1:]:
        ref_end = _find_section_end(lines, ref_pos)
        # Collect non-empty content lines from this section (skip the heading itself)
        for i in range(ref_pos + 1, ref_end):
            if lines[i].strip():
                extra_content.append(lines[i])
        sections_to_remove.append((ref_pos, ref_end))

    # Remove duplicate sections (in reverse order to preserve indices)
    new_lines = list(lines)
    for start, end in reversed(sections_to_remove):
        del new_lines[start:end]

    # Insert extra content at end of first reference section
    if extra_content:
        # Find the new end of first reference section
        new_first_end = _find_section_end(new_lines, first_ref)
        for i, content_line in enumerate(extra_content):
            new_lines.insert(new_first_end + i, content_line)

    return new_lines, ["merged duplicate Reference sections"] if sections_to_remove else []


def _find_section_end(lines, heading_idx):
    """Find the index where a section ends (next heading or end of file)."""
    for i in range(heading_idx + 1, len(lines)):
        if _is_any_heading(lines[i]):
            return i
    return len(lines)


def _handle_pldb_links(lines, concept):
    """Replace existing PLDB links or add new one to Reference section."""
    changes = []

    if concept is None:
        # No match - just remove any stale PLDB links (shouldn't normally happen)
        return lines, changes

    pldb_line = make_pldb_link_line(concept)

    # Check if there's already a PLDB link
    has_pldb_link = False
    has_correct_link = False
    pldb_indices = []

    for i, line in enumerate(lines):
        if PLDB_LINK_RE.match(line):
            pldb_indices.append(i)
            has_pldb_link = True
            if line.strip() == pldb_line:
                has_correct_link = True

    if has_correct_link and len(pldb_indices) == 1:
        return lines, []  # Already correct, nothing to do

    if has_pldb_link:
        # Replace all PLDB link lines with the correct one (keep first occurrence, remove rest)
        new_lines = []
        replaced = False
        for i, line in enumerate(lines):
            if i in pldb_indices:
                if not replaced:
                    new_lines.append(pldb_line)
                    replaced = True
                # else: skip duplicate PLDB lines
            else:
                new_lines.append(line)
        changes.append(f"updated PLDB link -> {concept}")
        return new_lines, changes

    # No existing PLDB link - need to add one to ## Reference section
    # (Will be handled by _ensure_reference_section if no Reference section exists)
    ref_idx = None
    for i, line in enumerate(lines):
        if _is_reference_heading(line):
            ref_idx = i
            break

    if ref_idx is not None:
        # Find where to insert - after the heading line, after any blank line
        insert_idx = ref_idx + 1
        # Skip blank lines after heading
        while insert_idx < len(lines) and lines[insert_idx].strip() == "":
            insert_idx += 1

        # Insert at the beginning of the reference section content
        new_lines = list(lines)
        new_lines.insert(insert_idx, pldb_line)
        changes.append(f"added PLDB link ({concept})")
        return new_lines, changes

    # No Reference section yet - will be added by _ensure_reference_section
    return lines, changes


def _ensure_reference_section(lines, concept):
    """Ensure ## Reference section exists. Add if missing."""
    changes = []

    # Check if ## Reference already exists
    for line in lines:
        if _is_reference_heading(line):
            return lines, []  # Already exists

    pldb_line = make_pldb_link_line(concept) if concept else None

    # Find a good place to insert ## Reference
    # Strategy: insert before ## License if it exists, otherwise at end
    insert_idx = None
    for i, line in enumerate(lines):
        stripped = line.strip().lower()
        if stripped.startswith("## license"):
            insert_idx = i
            break

    new_section = ["## Reference", ""]
    if pldb_line:
        new_section.insert(1, pldb_line)
        changes.append(f"added Reference section with PLDB link ({concept})")
    else:
        changes.append("added Reference section")

    new_section.append("")  # blank line after section

    new_lines = list(lines)
    if insert_idx is not None:
        # Ensure blank line before new section
        if insert_idx > 0 and new_lines[insert_idx - 1].strip() != "":
            new_lines.insert(insert_idx, "")
            insert_idx += 1
        for i, sec_line in enumerate(new_section):
            new_lines.insert(insert_idx + i, sec_line)
    else:
        # Append at end
        # Strip trailing blank lines first
        while new_lines and new_lines[-1].strip() == "":
            new_lines.pop()
        new_lines.append("")
        new_lines.extend(new_section)

    return new_lines, changes


def update_scroll_file(grammar_dir, concept, dry_run=False):
    """Phase 3: Update .scroll file in PLDB with antlr entry."""
    scroll_path = os.path.join(PLDB_CONCEPTS_DIR, concept + ".scroll")
    if not os.path.isfile(scroll_path):
        return None  # Shouldn't happen, but be safe

    with open(scroll_path, "r", encoding="utf-8", errors="replace") as f:
        original = f.read()

    correct_url = f"{ANTLR_BASE_URL}/{grammar_dir}"
    correct_line = f"antlr {correct_url}"

    lines = original.split("\n")

    # Check if antlr entry already exists
    antlr_idx = None
    for i, line in enumerate(lines):
        if line.startswith("antlr "):
            antlr_idx = i
            break

    if antlr_idx is not None:
        existing_url = lines[antlr_idx].strip().split(" ", 1)[1]
        if existing_url == correct_url:
            return None  # Already correct

        # Only fix if it points to grammars-v4
        if "antlr/grammars-v4" in existing_url:
            lines[antlr_idx] = correct_line
            new_content = "\n".join(lines)
            if not dry_run:
                with open(scroll_path, "w", encoding="utf-8", newline="\n") as f:
                    f.write(new_content)
            return f"FIX {concept}.scroll: antlr URL -> {correct_url}"
        else:
            return None  # External URL (e.g. ini -> yaip), don't touch
    else:
        # No antlr entry - append after the last simple key-value line in the header area
        insert_idx = _find_scroll_insert_position(lines)
        lines.insert(insert_idx, correct_line)
        new_content = "\n".join(lines)
        if not dry_run:
            with open(scroll_path, "w", encoding="utf-8", newline="\n") as f:
                f.write(new_content)
        return f"ADD {concept}.scroll: antlr {correct_url}"


def _find_scroll_insert_position(lines):
    """Find the best position to insert an antlr entry in a .scroll file.

    Strategy: Find the last simple key-value line (no leading spaces, not blank,
    not the first line which is the import) in the header area before the first
    complex section (indicated by indented lines or known multi-line markers).
    """
    # Look for patterns like "key value" - simple metadata lines
    # The header area typically ends when we hit sections with indented content
    last_kv_idx = 0
    in_header = True

    for i, line in enumerate(lines):
        if i == 0:
            continue  # Skip import line

        stripped = line.strip()

        # Blank line in early area is fine
        if not stripped:
            continue

        # Indented line means we're in a sub-section
        if line.startswith(" ") or line.startswith("\t"):
            continue

        # Known multi-line sections that indicate we're past simple key-value pairs
        if stripped.startswith("pygmentsHighlighter") or \
           stripped.startswith("wikipedia ") or \
           stripped.startswith("example") or \
           stripped.startswith("keywords ") or \
           stripped.startswith("lineComment") or \
           stripped.startswith("multiLine") or \
           stripped.startswith("has") or \
           stripped.startswith("githubBigQuery") or \
           stripped.startswith("semanticScholar") or \
           stripped.startswith("githubLanguage"):
            break

        # Simple key-value line
        if " " in stripped or stripped.startswith("isOpenSource") or stripped.startswith("fileType"):
            last_kv_idx = i

    # Insert after the last key-value line
    return last_kv_idx + 1


def pick_primary_grammar_dir(grammar_dirs, concept, mapping):
    """When multiple grammar dirs map to the same concept, pick the best one for the antlr entry.

    Priority:
    1. The one already referenced in the existing antlr entry
    2. The shortest path (most general)
    3. The one matching the concept name
    """
    # Check existing antlr entry
    scroll_path = os.path.join(PLDB_CONCEPTS_DIR, concept + ".scroll")
    if os.path.isfile(scroll_path):
        with open(scroll_path, "r", encoding="utf-8", errors="replace") as f:
            for line in f:
                if line.startswith("antlr ") and "antlr/grammars-v4" in line:
                    m = re.search(r"grammars-v4/(?:tree|blob)/master/(.+?)(?:\s|$)", line)
                    if m:
                        existing_path = m.group(1).rstrip("/")
                        if existing_path.endswith(".g4"):
                            existing_path = os.path.dirname(existing_path).replace("\\", "/")
                        if existing_path in grammar_dirs:
                            return existing_path
                    break

    # Sort by path length, then alphabetically
    sorted_dirs = sorted(grammar_dirs, key=lambda d: (len(d), d))
    return sorted_dirs[0]


def main():
    dry_run = "--dry-run" in sys.argv

    if dry_run:
        print("=== DRY RUN MODE - No files will be modified ===\n")

    # Phase 1: Build mapping
    print("Phase 1: Building grammar-to-PLDB mapping...")
    grammar_dirs = find_grammar_dirs()
    print(f"  Found {len(grammar_dirs)} grammar directories")

    mapping, unmatched = build_grammar_to_concept_mapping(grammar_dirs)
    print(f"  Matched: {len(mapping)} grammars")
    print(f"  Unmatched: {len(unmatched)} grammars")

    if unmatched:
        print("\n  Unmatched grammars:")
        for gdir in unmatched:
            print(f"    - {gdir}")

    # Group by concept to find duplicates
    concept_to_dirs = {}
    for gdir, concept in mapping.items():
        concept_to_dirs.setdefault(concept, []).append(gdir)

    # Pick primary grammar dir for each concept (for scroll file updates)
    primary_dirs = {}
    for concept, dirs in concept_to_dirs.items():
        primary = pick_primary_grammar_dir(dirs, concept, mapping)
        primary_dirs[concept] = primary

    # Phase 2: Update readmes
    print("\nPhase 2: Updating readmes in grammars-v4...")
    readme_changes = []
    for gdir in grammar_dirs:
        concept = mapping.get(gdir)
        result = update_readme(gdir, concept, dry_run)
        if result:
            readme_changes.append(result)
            print(f"  {result}")

    print(f"\n  Total readme changes: {len(readme_changes)}")

    # Phase 3: Update scroll files
    print("\nPhase 3: Updating .scroll files in PLDB...")
    scroll_changes = []
    for concept, primary_gdir in sorted(primary_dirs.items()):
        result = update_scroll_file(primary_gdir, concept, dry_run)
        if result:
            scroll_changes.append(result)
            print(f"  {result}")

    print(f"\n  Total scroll changes: {len(scroll_changes)}")

    # Summary
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
