# Java Class File Grammar

An ANTLR4 grammar for binary Java `.class` files, not Java source or `javap`
text. The format is defined in the
[Java Virtual Machine Specification, chapter 4](https://docs.oracle.com/javase/specs/jvms/se25/html/jvms-4.html).

The `clazz` entry point consumes the entire file: magic number, versions,
constant pool, access flags, class references, interfaces, fields, methods,
and attributes. All standard constant-pool tags through Java 25 are recognized,
including dynamic constants, invokedynamic, modules, and packages. Long and
double constants occupy two constant-pool slots.

This is a **Java-target** parser. Java actions and predicates enforce the
unsigned big-endian counts and byte lengths in the binary format; do not
generate other targets without porting these actions. Decode input as
**ISO-8859-1**, which maps each input byte to one character without losing
bytes or interpreting UTF-8 sequences. Do not skip whitespace or NUL bytes.
The descriptor configures this encoding for Trash Toolkit drivers.

Attribute bodies (including `Code`) and modified UTF-8 string payloads are
retained as bytes rather than decoded. This is structural parsing, not JVM
verification: constant-pool references, descriptors, access-flag combinations,
version restrictions, modified UTF-8 validity, and bytecode correctness require
separate validation.

## Examples and testing

All ten binary files in [`examples/`](examples/) are tested, including the
original Java 7 `CBean.class`. The other nine are compiled from the sources in
[`example-sources/`](example-sources/), covering fields and primitive constants,
interfaces, enums, annotations, generics, exceptions, lambdas, records, and
module metadata. Sources are outside `examples/` so only binary inputs are parsed.

Run `mvn -pl clazz clean test` from the repository root. To regenerate the nine
fixtures with JDK 17 or newer, run from this directory:

```sh
javac --release 17 -g -parameters -d examples example-sources/*.java
```
