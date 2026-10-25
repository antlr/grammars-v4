# SPICE grammar

An ANTLR 4 grammar for a practical subset of
[SPICE](https://en.wikipedia.org/wiki/SPICE), the circuit simulation netlist
language. Syntax reference: the [ngspice manual](https://ngspice.sourceforge.io/docs/ngspice-manual.pdf).

The grammar handles case-insensitive component and directive lines, numeric
or named nodes, engineering suffixes, scientific notation, source waveforms,
model parameters, subcircuits, continuation lines, quoted filenames, and
arithmetic in braced expressions. Multiplication and division bind more tightly
than addition and subtraction. Powers use `^` and associate to the right.

Full-line comments begin with `*`; inline comments begin with `;`. Line endings
are significant, with LF and CRLF supported and an optional final newline.
Conventional `name.cir ...` title lines and comment titles are supported; arbitrary
free-text titles and simulator-specific control languages are not.

This is a syntax parser, not a circuit validator or simulator. Component pin
counts, directive names, model parameters, engineering suffix meanings, and
subcircuit references are not validated.

All ten files in [`examples/`](examples/) are parsed using the `netlist` entry
point, which consumes the entire input. Run `mvn -pl spice clean test` from the
repository root, or `mvn clean test` from this directory. `desc.xml` configures
the same entry point for Trash Toolkit drivers.
