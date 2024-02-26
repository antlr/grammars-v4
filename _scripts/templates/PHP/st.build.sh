# Generated from trgen <version>
set -e

if [ -f transformGrammar.py ]; then python3 transformGrammar.py ; fi

# Because there is no integrated build script for Dart targets, we need
# to manually look at the version in pubspec.yaml and extract the
# version number. We can then use this with antlr4 to generate the
# parser and lexer.

# This table lookup is caused because the PHP does not
# use the same version number as the tool.

# Find runtime version in composer.json file.
runtime_version=`grep antlr4 composer.json | awk '{print $2}' | tr -d '\r' | tr -d '\n' | tr -d ',' | tr -d '"'`

# Get from online sources the commit version number that corresponds to the runtime version.
rm -rf antlr4-php-runtime
wget "https://packagist.org/packages/antlr/antlr4-php-runtime#$runtime_version"
commit_version=`grep BSD-3-Clause antlr4-php-runtime | awk '{print $NF}' | sed 's/\<.*//'`

# Checkout the sources from the php runtime repo.
rm -rf antlr4-php-runtime
git clone https://github.com/antlr/antlr-php-runtime
cd antlr-php-runtime
git checkout $commit_version

# Extract the tool version this damn runtime version corresponds to.
tool_version=`grep 'public const VERSION' src/RuntimeMetaData.php | awk '{print $NF}' | sed "s/'//g" | sed 's/;//'`
cd ..
rm -rf antlr4-php-runtime

<tool_grammar_tuples:{x |
antlr4 -v $tool_version -encoding <antlr_encoding> -Dlanguage=PHP <x.AntlrArgs> <antlr_tool_args:{y | <y> } > <x.GrammarFileName>
}>

composer install

exit 0
