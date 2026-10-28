# PDB Grammar

An ANTLR 4 record-level grammar for the
[Protein Data Bank (PDB) file format](https://en.wikipedia.org/wiki/Protein_Data_Bank_(file_format)),
which stores macromolecular structure and associated metadata.

Reference: [wwPDB format version 3.3](https://www.wwpdb.org/documentation/file-format-content/format33/v3.3.html).

The grammar recognizes standard metadata, sequence, secondary structure,
crystallographic, coordinate, connectivity, and bookkeeping record names.
Each record is retained as a single token, preserving spaces and the fixed-column
payload. LF and CRLF are supported, and the final newline is optional.
Record names must appear at the start of a line; unknown record names, tabs,
and trailing unrecognized input produce errors.

This is a record parser, not a complete PDB validator. It does not validate
individual column widths, numeric fields, record ordering, atom references,
or molecular geometry. It does not parse the separate PDBx/mmCIF format.

The ten small synthetic inputs in [`examples/`](examples/) exercise record
families; they are parser fixtures rather than deposited experimental structures.
The `file_` entry point requires at least one record and consumes the entire
input. Run all examples with `mvn -pl pdb clean test` from the repository root,
or `mvn clean test` from this directory. `desc.xml` configures the same entry
point for Trash Toolkit drivers.