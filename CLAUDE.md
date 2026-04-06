# Claude Notes

@GLOSSARY.md

## Trash Toolkit

The Trash Toolkit is a collection of .NET tools for working with Antlr4 grammars.
Source and documentation: https://github.com/kaby76/Trash

The tools are declared in `.config/dotnet-tools.json` at the repo root and must be
restored once before use (or after a fresh clone):

```sh
cd <repo-root>       # i.e. the grammars-v4 clone root
dotnet tool restore
```

This installs all `tr*` tools (`trgen`, `trwdog`, `trperf`, `trglob`, `triconv`,
`trparse`, `trquery`, `trxml2`, etc.) as local .NET tools available via `dotnet <toolname>`.

## Testing a Grammar

To test a grammar (e.g. `csharp/v8-spec`), follow these steps from the repo root.

**1. Restore the Trash Toolkit (once per clone), and note location:**

```sh
dotnet tool restore
cloneroot=`pwd`
```

**2. Generate the C# sandbox:**

```sh
cd csharp/v8-spec
dotnet trgen -t CSharp --template-sources-directory $cloneroot
```

`trgen` reads `desc.xml` in the current directory for grammar and test configuration,
discovers all `.g4` files, resolves their dependencies, and writes the complete driver
application into `Generated-CSharp/`.

**3. Build the driver:**

```sh
cd Generated-CSharp
bash build.sh
```

`build.sh` runs `antlr4` to regenerate the lexer/parser C# sources from the `.g4`
files, then `dotnet build` to compile the driver. You will need to install prerequisites
for building and compiling the generated driver.

**4. Run the test suite, and test individual parses:**

```sh
bash test.sh
```

`test.sh` parses all example files listed in `desc.xml`, writes `.tree` and `.errors`
files alongside the inputs, and diffs them against the committed baselines. It prints
`Test succeeded.` on success or `Test failed.` with a diff on failure.

```sh
bash run.sh *.txt
```

`run.sh` parses individual files.

## Checking for Ambiguity (Trash Toolkit / C# target)

The generated C# test harness is in `.../Generated-CSharp/`.

**Build** (only needed once, or after grammar changes):

```sh
cd Generated-CSharp
bash build.sh
```

**Check a single file for ambiguity:**

Using the built binary directly:

```sh
./bin/Debug/net10.0/Test.exe --ambig <file>
```

The `--ambig` flag enables ANTLR's profiling mode. After parsing, it reports every
ATN decision where more than one alternative was viable for the actual input (a true
grammar ambiguity). For each ambiguity it prints all distinct parse trees, one per
line, prefixed with `d=<decision>.a=<alt>` (e.g. `d=195.a=1`, `d=195.a=2`).

**Restrict output to specific decisions:**

```sh
./bin/Debug/net10.0/Test.exe --ambig=195,42 <file>
```

**Limit the number of parse trees returned:**

```sh
./bin/Debug/net10.0/Test.exe --ambig --limit=4 <file>
```

**Using trparse/trtree to show ambiguities:**

```sh
dotnet trparse --ambig <file> | dotnet trtree
```
