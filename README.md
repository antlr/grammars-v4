[![Travis Status](https://api.travis-ci.org/antlr/grammars-v4.png)](https://travis-ci.org/antlr/grammars-v4)

# grammars-v4

Grammars written for [ANTLR v4](https://github.com/antlr/antlr4)

This repository is a collection of Antlr4 grammars.   

The root directory name is the all-lowercase name of the language parsed by the grammar. For example, java, cpp, csharp, c, etc...

## Examples

* [Java Example](https://github.com/teverett/antlr4example)
* [C++ Example](https://github.com/teverett/antlr4-cpp-example)

## Contributing

The grammars-v4 tree uses Travis in conjunction with [antlr4test-maven-plugin](https://github.com/antlr/antlr4test-maven-plugin) to test all grammars against example inputs. This ensures that all grammars on the tree parse, compile and work with the example inputs.

Therefore:

* Pull requests which cause test failures will be rejected
* Pull requests which include additional examples to be used as test cases are strongly preferred

## License

Beware of the licenses on the individual grammars. There is no common license. When in doubt or you don't know what you're doing, please use the BSD or MIT license.

## Testing the Grammars

```
mvn clean test
```
