# YINI ANTLR 4 grammar

This grammar is based on official language specification hosted at [YINI-spec on GitHub](https://github.com/YINI-lang/YINI-spec).

Includes both the lexer and parser of the grammar.

The entry rule is `yini`.

## What is YINI?

**YINI (Yet another INI)** is a minimal and human-readable configuration file format with a formally defined grammar and a specification. It was designed for clarity and simplicity, offering features that improve on classic INI while avoiding the complexity of formats like YAML - yet being less noisy than JSON and TOML.

YINI is clean, consistent, and structured — easy for humans to write and machines to parse.

> YINI aims to hit the sweet spot between human-friendly simplicity and reliable structure — without the noise of JSON or the quirks of YAML.

Inspired by including INI, JSON, Python, and Markdown. YINI keeps things minimal and consistent — with **structured sections**, **multiple comment styles**, and a **formal grammar**.

**TL;DR:**
- ✅ **Combines structure and simplicity** — more expressive than INI, less verbose than JSON, YAML, or TOML. 
- ✅ **Minimal syntax, maximal readability** — aiming for clarity over cleverness.

---

## Key Features
YINI aims to prioritize **human readability, clarity, and clean syntax**.

- ✔️ **Clean and minimalistic syntax** — avoids visual noise, easy to write and read.
- ✔️ **Typing support** for: Strings, Numbers, Booleans, Lists (Arrays), and Nulls.
- ✔️ **Easy section nesting** Markdown-style section levels: `^`, `^^`, `^^^` ...
- ✔️ **Indentation-independent structure** — no indentation pitfalls.
- ✔️ **Flexible Commenting styles** — C-style commenting rules using `//` and `/* ... */`. Supports `#` and `;` commenting styles too. 
- ✔️ **Flexible Literals:**
  * Including booleans: `true`, `false`, `on`, `off`, `yes`, `no` (all case-insensitive).
  * Numeric notations with base and exponent support.
- ✔️ **Human readability first** — prioritizes clarity over cleverness — yet machine-friendly.

### Quick Code Example
Example in JavaScript:
```js
import YINI from 'yini-parser';

const config = YINI.parse(`
    ^ App
    title = "My App"
`);

console.log(config);
```

### YINI Parser Available

Open-source **YINI parser for Node.js & TypeScript** is available:

- **GitHub:** [yini-parser-typescript](https://github.com/YINI-lang/yini-parser-typescript)
- **npm:** [yini-parser](https://www.npmjs.com/package/yini-parser)

---

## Reference


## License

YINI is licensed under the [Apache License 2.0](./license).

---

## Known Limitation

Although this grammar is based on the official language specification, some rules allow a wider range of tokens (i.e., more relaxed rules) to enable implementing parsers to catch and report errors more effectively.