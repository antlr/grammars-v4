---
name: add-new-grammar
description: Use this skill whenever a new ANTLR4 grammar is being added to this grammars-v4 repo (a new top-level grammar directory, or finishing off one that's already partially there). It walks through the four things every grammar directory needs to be considered complete and mergeable here — at least 10 working example inputs, registration in the root pom.xml, a valid desc.xml, and a README.md linking to the language's spec with every link checked to actually resolve — and checks each one against the repo's actual build/test tooling rather than just eyeballing files. Trigger on phrases like "add a new grammar", "add support for <language>", "finish the <foo> grammar", or "is this grammar ready to merge".
---

# Adding a new grammar to grammars-v4

A grammar directory in this repo isn't done just because the `.g4` files parse. Four
things are checked in PRs and CI: enough real example coverage, module registration,
a valid `desc.xml`, and a README that tells a reader what language this is and where
its spec lives. Go through all four for every new (or newly-finished) grammar
directory — use a recently-merged grammar like `natural/` as your reference shape if
anything here is ambiguous, since conventions drift slightly over time and a recent
example reflects current practice better than an old one.

## 0. Know where things stand

```sh
ls <grammar-dir>/
```

A new grammar usually has the `.g4` file(s) already written and maybe a handful of
example inputs. Figure out what's missing from the four checks below before doing
any work — don't redo what's already there.

## 1. At least 10 working examples

Example inputs live in `<grammar-dir>/examples/`, one file per example, using
whatever extension is idiomatic for the language (`.nsp` for NATURAL, `.txt` is a
safe fallback when the language has no common extension). "Working" means: parses
successfully through this repo's actual test harness, not just "looks plausible."
Follow the root `CLAUDE.md` "Testing a Grammar" steps to verify this for real:

```sh
cd <grammar-dir>
dotnet trash gen -t CSharp      # reads desc.xml, writes Generated-CSharp/
cd Generated-CSharp
bash build.sh                   # runs antlr4, then dotnet build
bash test.sh                    # parses every example, diffs against baselines
```

`test.sh` prints `Test succeeded.` when every example parses and matches its
committed `.tree`/`.errors` baseline. If an example is meant to legitimately fail
to parse (testing error recovery, say), that's fine as long as the corresponding
`.errors` baseline is committed and non-empty — see the "no errors found, but
errors file exists" pitfall in `CLAUDE.md` if you hit a confusing Maven failure
about zero-length `.errors` files.

Aim for examples that exercise meaningfully different grammar paths — different
statement types, nesting, edge-case syntax — not 10 trivial variations of the same
one-liner. Ten examples that all hit the same two rules don't tell you much about
whether the grammar actually works.

## 2. Registered in the root pom.xml

```sh
grep -n "<module>" pom.xml | less
```

The `<modules>` list in the repo-root `pom.xml` is kept alphabetically sorted by
directory name. Add `<module><grammar-dir></module>` in alphabetical position
(check the artifactId/module name matches the directory name exactly — that's what
Maven resolves against). Without this line, `mvn clean test` from the repo root
silently skips the new grammar entirely, so it's easy to forget and have nothing
complain about it until someone else notices.

Also check `<grammar-dir>/pom.xml` itself exists and follows the shape every other
grammar uses — `antlr4-maven-plugin` for codegen and `antlr4test-maven-plugin`
(`com.khubla.antlr`) for testing, both inheriting version numbers from the parent
POM rather than hardcoding them. Set `<entryPoint>` to the grammar's actual start
rule, `<exampleFiles>examples/</exampleFiles>`, and `<testFileExtension>` to match
whatever extension step 1's examples use. If this file is missing entirely, copy
the shape from a recent grammar directory rather than writing one from scratch.

## 3. A valid desc.xml

`desc.xml` is what `trgen` (the Trash Toolkit) reads to know which language targets
to generate drivers for. Nearly every grammar in this repo uses the same list — copy
it rather than inventing a shorter one, since narrowing it deliberately excludes the
grammar from being tested against the ambiguity-checking and other target-specific
Trash tooling:

```xml
<?xml version="1.0" encoding="UTF-8" ?>
<desc xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:noNamespaceSchemaLocation="https://github.com/kaby76/Trash/raw/refs/heads/main/desc.xsd">
   <targets>Antlr4ng;Cpp;CSharp;Dart;Go;Java;JavaScript;Python3;TypeScript</targets>
</desc>
```

Validate it actually works, rather than just checking it parses as XML, by running
step 1's `dotnet trash gen -t CSharp` and confirming `Generated-CSharp/` comes out —
a `desc.xml` with a typo'd target name or malformed XML will fail there even though
the file "looks fine."

## 4. README.md with a link to the authoritative spec

Every grammar directory needs a `README.md` so a reader lands somewhere that tells
them what the language is, before they ever open the `.g4` file. The convention,
visible across the repo, is:

```markdown
# <LANGUAGE> Grammar

[<Language>](<homepage-or-wikipedia-link>) is <one or two sentences of what it is
and why it exists / where it's used>.

## Source

<link to the organization or project that owns the language>

## Specification

<link to the actual language reference/spec/manual — this is the link a grader or
future maintainer will click to check the grammar against the real rules>

## Notes

<anything a maintainer needs to know that isn't obvious from the grammar itself:
scope limitations, deliberately unimplemented dialects/modes, ambiguities left
unresolved and why, quirks in how comments/whitespace are handled>

Entry rule: `<start rule name>`
```

The Specification link is the part most worth getting right — prefer a stable,
official reference over a blog post or tutorial, and if the authoritative spec is
paywalled, hard to find, or you couldn't actually verify the grammar against it,
say so explicitly in Notes (don't just silently link something tangential). See
`natural/README.md` for what that honesty looks like when the official docs turned
out to be unreachable.

### Check every link actually resolves

A dead link in a README is worse than no link — it looks authoritative while
sending the next reader nowhere. Before calling the README done, fetch every URL
in it (Source, Specification, and any others in Notes) and confirm it returns a
real page rather than a 404, DNS failure, or a redirect to an unrelated domain —
use WebFetch or a plain HTTP check for each one:

```sh
for url in $(grep -oE 'https?://[^) ]+' README.md); do
  code=$(curl -s -o /dev/null -w '%{http_code}' -L "$url")
  echo "$code  $url"
done
```

A redirect (3xx followed by a 200) is fine; a terminal 4xx/5xx or a connection
failure isn't — swap in a working mirror of the same document if one exists, or
otherwise do what `natural/README.md` does and say plainly in Notes that the
official page couldn't be reached, rather than leaving a link that quietly rots.
Re-check this if you revisit a README later, since even a previously-good spec
link can go stale.

## Before calling it done

Re-run `bash test.sh` (and, if there's an existing root-level `mvn clean test` habit
in this repo, that too) one more time after all four pieces are in place — adding
the README or pom.xml entry doesn't typically break parsing, but it's cheap
insurance against a last-minute typo in `desc.xml` or the `pom.xml` module name.
Re-run the link check from step 4 too if the README changed after you last checked
it.
