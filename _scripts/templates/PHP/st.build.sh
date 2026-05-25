# Generated from trgen <version>
set -e

if [ -f transformGrammar.py ]; then python3 transformGrammar.py ; fi

<if(antlr_is_dev)>
ANTLR4_DEV_DIR=<antlr_dev_dir>/antlr4
if [ ! -d "$ANTLR4_DEV_DIR/.git" ]; then
  git clone --quiet https://github.com/antlr/antlr4.git "$ANTLR4_DEV_DIR"
fi
(cd "$ANTLR4_DEV_DIR" && git checkout dev && git pull && mvn -DskipTests install)
JAR=$(ls "$ANTLR4_DEV_DIR"/tool/target/antlr4-*-complete.jar | tail -1)
<else>
# Because there is no integrated build script for Dart targets, we need
# to manually look at the version in pubspec.yaml and extract the
# version number. We can then use this with antlr4 to generate the
# parser and lexer.

# This table lookup is caused because the PHP does not
# use the same version number as the tool.

# Find runtime version in composer.json file.
runtime_version=`grep antlr4 composer.json | awk '{print $2}' | tr -d '\r' | tr -d '\n' | tr -d ',' | tr -d '"'`

# Get from online sources the commit version number that corresponds to the runtime version.
rm -rf antlr4-php-runtime antlr-php-runtime
wget "https://packagist.org/packages/antlr/antlr4-php-runtime#$runtime_version"
commit_version=`grep BSD-3-Clause antlr4-php-runtime | awk '{print $NF}' | sed 's/\<.*//'`

# Checkout the sources from the php runtime repo.
rm -rf antlr4-php-runtime antlr-php-runtime
git clone https://github.com/antlr/antlr-php-runtime
cd antlr-php-runtime
git checkout $commit_version

# Extract the tool version this damn runtime version corresponds to.
tool_version=`grep 'public const VERSION' src/RuntimeMetaData.php | awk '{print $NF}' | sed "s/'//g" | sed 's/;//'`
cd ..
rm -rf antlr4-php-runtime
<endif>

<if(antlrng_tool)>
npm init -y
npm i antlr-ng
<endif>

<tool_grammar_tuples:{x |
<if(antlrng_tool)>
tsx $HOME/antlr-ng/cli/runner.ts --encoding <antlr_encoding> -Dlanguage=PHP <x.AntlrArgs> <antlr_tool_args:{y | <y> } > <x.GrammarFileNameTarget>
<elseif(antlr_is_dev)>
java -jar "$JAR" -encoding <antlr_encoding> -Dlanguage=PHP <x.AntlrArgs> <antlr_tool_args:{y | <y> } > <x.GrammarFileNameTarget>
<else>
antlr4 -v $tool_version -encoding <antlr_encoding> -Dlanguage=PHP <x.AntlrArgs> <antlr_tool_args:{y | <y> } > <x.GrammarFileNameTarget>
<endif>
}>

composer install

exit 0
