# Erlang grammar

## Source

[https://bitbucket.org/fenollp/erlang-grammar](https://bitbucket.org/fenollp/erlang-grammar)

## Overview

An unofficial Erlang grammar adaptation in **ANTLR v4**.
Typically, the only one on the web.

It is merely extracted from the [official non-standard grammar from Erlang/OTP sources](https://github.com/erlang/otp/blob/maint/lib/stdlib/src/erl_parse.yrl).

It is meant for students and people who work on Erlang's syntax.


## Requierements

* [antlr4](http://www.antlr.org/wiki/display/ANTLR4/Getting+Started+with+ANTLR+v4)
* Erlang's **erlc** for preprocessing purposes.

## Usage

Generate the grammar from Erlang.g4 with:

```
:::bash
make clean all
```

Live debug with tree representation:

```
:::bash
make debug
…type Erlang forms…
^D
```

Preprocess files in src/ and check Erlang.g4 against preprocessed files in examples/:

```
:::bash
./check.sh src examples
```

Check Erlang.g4 against the whole Erlang/OTP's `.erl` sources:

```
:::bash
make check
```