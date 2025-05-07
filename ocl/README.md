# OCL Antlr Grammar

This repository provides grammars for the Object Constraint Language (OCL) 2.4, along with its integration with UML-RSDS, a tool supporting UML 2.5 class diagram notation and OCL 2.4. These grammars aim to enhance the precision and usability of OCL while making it easier to integrate with UML modeling tools.

## Overview

The repository is structured around one main grammar: OCL. However, it has been split into two files to separate the core OCL grammar from the UML-RSDS specification tool that also includes UML. The separation makes it easier to use and understand the grammars while still allowing their combined use when needed.

### Key Features

- **OCL Core Grammar (`OCL.g4`)**: This file contains the core OCL 2.4 grammar, providing a consistent and robust foundation for working with OCL expressions.
- **UML-RSDS Grammar (`UMLRSDS.g4`)**: This file integrates the OCL core grammar with UML-RSDS, enabling the specification of UML models using OCL expressions.

These grammars are designed to support the consistent use of the arrow operator (`->`) for operations on non-primitive types (e.g., strings, collections, functions, maps). For example:

- `str->isMatch(patt)` (for strings `str` and `patt`)

However, string concatenation remains an exception to this rule:

- `s1 + s2`

Boolean and numeric operators retain their traditional infix or prefix notations:

```ocl
a xor b
a * bm div netc.
```

### Composition

Using OCL without a tool grammar to specify the UML model is often impractical. While the grammars are modular, they are designed to work seamlessly together:

- `OCL.g4` provides the foundation for OCL expressions.
- `UMLRSDS.g4` integrates the core OCL grammar to enable UML modeling.

The repository’s structure ensures flexibility, allowing users to utilize either grammar independently or together, depending on their needs.

### Future Contributions

This repository currently focuses on the OCL core and UML-RSDS grammars. In the future, there is potential to include grammars from other tools, such as USE-OCL, to further expand the repository’s utility and scope.

## Main Contributors

* Kevin Lano, 2022

## References

- OCL 2.4 Specification (OMG OCL 2.4 PDF) https://www.omg.org/spec/OCL/2.4/PDF
- UML-RSDS Tool (Supports UML 2.5 and OCL 2.4):
    - UML-RSDS at [WIKIPEDIA](https://en.wikipedia.org/wiki/UML-RSDS)
    - Lano, K. (2016). Agile Model-Based Development Using UML-RSDS (1st ed.). CRC Press. https://doi.org/10.1201/9781315368153

---

Feel free to contribute to this repository or suggest additional grammars and tools that could enhance its capabilities.
