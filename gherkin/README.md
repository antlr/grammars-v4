# Background

Gherkin is the protocol used by the Cucumber test framework for "Business Driven Development". It is a domain specific language for defining business scenarios in natural languages which in turn can be turned into automated tests.

## Updating the parser

The parser here was created and shared from the [PolymorphicDSL Test Framework](https://github.com/google/polymorphicDSL/tree/main/src/main/antlr4/com/pdsl/gherkin/parser) which also uses supports Gherkin for creating tests. This is a viable source of truth if you need to get updates as it aspires to support all gherkin features.

The lexer is rather large because Gherkin aspires to support all natural languages (English, French, Spanish, etc). They even have some toy languages for "pirate" and a comical localization for Australian.

This parser was up to date at the time it was created in 2021 and there do not appear to be any major changes to the Gherkin spec since that time. 

## Updating the Lexer

The lexer is likely to be the thing that changes from year to year as the gherkin protocol expands to include more natural languages. This is cumbersome to do by hand. In the event new languages or keywords need to be added the best bet is probably to use the [PolymorphicDSL python script](https://github.com/google/polymorphicDSL/blob/main/src/main/antlr4/com/pdsl/gherkin/parser/gherkin_template_processor.py) designed to consume the JSON format that the gherkin project uses to define the keywords. The most recent JSON language file can be found [here](gherkin-languages.json).

## Examples

The examples were originally created by the original Cucumber framework's Gherkin repository [here](https://github.com/cucumber/gherkin/tree/08305501ee838d1e036f13306e1a9e745ac1f6a1/testdata/good)

Sadly, it seems most Cucumber implementations do not support the full Gherkin spec, including things like making "Scenario Outline" optional, the "Rule" keywords or even things as simple as multiline descriptors for scenarios. Using the above examples is the best way to keep the parser up to date. Ironically, it is "Business Driven Development" that lets you know clearly if you support the spec well, but most implementors seem to be unaware of it.




