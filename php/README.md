## Summary

PHP 7.4 grammar with C# ([Sharwell runtime](https://github.com/tunnelvisionlabs/antlr4cs)) 
and Java runtime by Ivan Kochurkin (kvanttt@gmail.com), Positive Technologies.
PHP keywords are case-insensitive, but tokens in grammar written in lower case.
Thus [CaseInsensitiveInputStream](https://gist.github.com/sharwell/9424666) should be used.
C# or Java code actions used for context-sensitivity features like Heredoc.

Parser grammar based on [Phalanger](https://github.com/DEVSENSE/Phalanger) grammar
by Jakub Míšek (jakubmisek).
Html mode based on [ANTLR html grammar](https://github.com/antlr/grammars-v4/tree/master/html)
by Tom Everett (@teverett).

Supported features:

* Different modes (because of PHP is [island grammar](https://en.wikipedia.org/wiki/Island_grammar)):
  * HTML
  * Script
  * CSS
  * PHP
  * Heredoc
* [Alternative syntax](http://php.net/manual/en/control-structures.alternative-syntax.php).
* [Heredoc](http://php.net/manual/en/language.types.string.php#language.types.string.syntax.heredoc).
* [Interpolation strings](http://php.net/manual/en/language.types.string.php#language.types.string.parsing.simple) (not fully completed, see TODO).
* Deep expressions handling (such as very long concatenation).
* aspTags.
* Improved lexer error processing with artificial string fragments
(for example double closed quote at the end: `<div attr='value'' />`).

## Testing

PHP parser has been successfully tested (parsing without errors) on the following projects.

* [phpBB-3.1.6](https://github.com/phpbb/phpbb/archive/release-3.1.6.zip)
* [ZendFramework-2.4.8](https://github.com/zendframework/zf2/archive/release-2.4.8.zip)

Also this parser has been tested on plenty number of PHP files from different CMS (~70000 files).
It took approximately 1 hour and 15 minutes with 70% on lexer part and 30% on parser part.

## License

[MIT](https://opensource.org/licenses/MIT)
