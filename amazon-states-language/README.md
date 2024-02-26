# Amazon States Language (ASL) ANTLR4 grammar

The ANTLR4 grammar for the Amazon States Language (ASL), initially developed by 
[LocalStack](github.com/localstack/localstack).

The module contains lexer and parser grammars (`ASLLexer.g4` and `ASLParser.g4`) for 
the Amazon States Language (ASL) language. Additionally, it includes grammars 
(`ASLIntrinsicLexer.g4` and `ASLIntrinsicParser.g4`) tailored for parsing intrinsic 
functions within the ASL derivations.

## License
```txt
Copyright (c) 2017+ LocalStack contributors
Copyright (c) 2016 Atlassian Pty Ltd

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
```

## Links

* [LocalStack](https://github.com/localstack/localstack) - LocalStack, the project 
that developed the grammar and an Amazon States Language interpreter.
* [Amazon Language Specification](https://states-language.net/spec.html) - The original 
language specification for Amazon States Language provided by Amazon.


## Examples
The `examples/state_machines` directory contains baseline examples demonstrating various types
of State Machine definitions in JSON. Additionally, within the `examples/intrinsic_functions` 
directory, there are examples illustrating the utilisation of intrinsic functions.

## Original source
<https://github.com/localstack/localstack>
