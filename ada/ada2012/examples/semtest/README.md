# Semantic Test: Cross-File Symbol Resolution

This directory contains a test case for verifying that the Ada parser correctly
resolves type names across files via `with` clauses.

## Files

- `helpers.ads` -- Package specification defining types (`Pair`, `Small_Int`,
  `Int_Array`, `Color`) and subprograms
- `helpers.adb` -- Package body implementing the subprograms
- `main.adb` -- Main procedure that `with`s and `use`s `Helpers`, then uses
  its types in qualified expressions, type conversions, and aggregates

## How It Works

When the parser encounters `with Helpers;` in `main.adb`, it:

1. Converts the package name to a filename using GNAT conventions:
   `Helpers` becomes `helpers.ads` (lowercase, dots become hyphens for child packages)
2. Searches for the `.ads` file in:
   - The directory of the file being parsed
   - Directories specified by `--I` options (in order)
3. Parses the `.ads` file with a sub-parser to build its symbol table
4. Imports the exported (non-predefined) symbols into the current parser's
   symbol table
5. Caches results so the same package is not re-parsed for other files

## Running

```bash
cd Generated-CSharp

# Parse main.adb with search path pointing to the semtest directory
bash run.sh --I../examples/semtest ../examples/semtest/main.adb

# Parse all three files together
bash run.sh --I../examples/semtest ../examples/semtest/helpers.ads ../examples/semtest/helpers.adb ../examples/semtest/main.adb

# With debug output to see symbol imports
bash run.sh --I../examples/semtest --debug ../examples/semtest/main.adb 2>&1 | grep -i 'import\|defined'
```
