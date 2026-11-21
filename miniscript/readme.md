# Miniscript

ANTLR4 grammar for Bitcoin Miniscript expressions.

## Notes

This grammar parses the surface syntax for Miniscript fragments and wrappers. It intentionally does not validate semantic constraints such as the Miniscript type system, threshold arity (`k <= n`), locktime compatibility, or descriptor-context key restrictions.

A few syntactic choices are worth calling out:

* Key expressions are opaque tokens. Concrete [BIP 380](https://github.com/bitcoin/bips/blob/master/bip-0380.mediawiki#key-expressions) key forms (hex public keys, WIF, extended keys with origins, derivation paths and multipath specifiers) all parse, but so do the symbolic key names (`pk(A)`, `multi(2,key_1,key_2)`) that BIP 379 itself and policy compilers use throughout. The grammar therefore does not enforce that a key is a well-formed BIP 380 key expression, nor context rules such as x-only keys under Taproot; that validation belongs downstream.
* Numbers are written in canonical form, without leading zeros or signs: `older(01)` and `older(+1)` are syntax errors. This is a lexical nuance on which implementations differ: rust-miniscript is equally strict, while Bitcoin Core's string parser still tolerates leading zeros (its `+`-sign acceptance was removed in bitcoin/bitcoin#30577).
* `multi()` enforces the legacy 20-key CHECKMULTISIG limit syntactically, while the key count of `multi_a()` is left unbounded. Bitcoin Core caps `multi_a()` at `MAX_PUBKEYS_PER_MULTI_A` (999) at parse time; that bound is treated here as a downstream semantic check like the other numeric range limits.

Whitespace is skipped for convenience when parsing human-authored inputs. Canonical serialized Miniscript is usually written without spaces.

## Reference

* [BIP 379: Miniscript](https://github.com/bitcoin/bips/blob/master/bip-0379.md) ([bips.dev mirror](https://bips.dev/379/))
* [Miniscript project site](https://bitcoin.sipa.be/miniscript/)
