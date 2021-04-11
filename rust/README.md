# Rust ANTLR 4 grammar

This grammar is based on official language reference.

Licensed under MIT

Entry rule is `crate`, you need run test via command line at the moment: `grun Rust crate examples/*.rs`.

Last updated for rust v1.49.0

## Maven build

Install the parser into the local Maven repository with `mvn install`.

## Known limitation

- Only v2018+ stable feature is implemented.
- Checks about isolated `\r` is not implemented. 
- `let a = b.0.1.2.3` is not supported due to conflict with float literal. (rust v1.46.0, https://github.com/rust-lang/rust/pull/71322/)
