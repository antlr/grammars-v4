#!/bin/bash

function usage_info() {
    cat <<USAGE
Usage: $0 [-h|--help] [--skip-build] <-g|--grammar <>> <file>
Options:
    -h, --help:             Print usage info and quit.
    --skip-build:           Default is false. Set to true when there are no changes in the ANTLR grammar.
    -g <>, --grammar <>:    The ANTLR grammar to use. Supported values are: ${!grammar_names[@]}
    file:                   Source file to parse.
USAGE
}

# TODO - Infer these values automatically, below is just a demonstration example.
declare -A grammar_names
grammar_names=(["solidity"]="Solidity")
declare -A grammar_start_rules
grammar_start_rules=(["solidity"]="sourceUnit")
declare -A grammar_source_files
grammar_source_files=(["solidity"]="Solidity.g4")

SCRIPT_DIR=$(cd ${0%/*} && pwd -P)
PROJECT_SRC_DIR=$SCRIPT_DIR/..
ANTLR_OUTPUT_DIRECTORY=$PROJECT_SRC_DIR/out

function antlr_lib_info() {
    cat <<ANTLR_LIB
Do the following:
    - Download the ANTLR JAR file (check out https://repo1.maven.org/maven2/org/antlr/antlr4/).
    - Set the file path to ANTLR_JAVA_LIB: "export ANTLR_JAVA_LIB=<path/to/antlr/jar/file>".
ANTLR_LIB
}

if [[ $# -eq 0 ]]; then
    usage_info
    exit 1
fi

SKIP_BUILD=false

while [ "$1" != "" ]; do
    case $1 in
    -h | --help)
        usage_info
        exit 0
        ;;
    --skip-build)
        SKIP_BUILD=true
        shift
        ;;
    -g | --grammar)
        shift
        GRAMMAR=$1
        shift
        ;;
    *)
        FILE=$1
        shift
        ;;
    esac
done

if [[ -z $GRAMMAR ]]; then
  echo "No grammar provided."
  usage_info
  exit 1
else
  echo "Using grammar $GRAMMAR."
  GRAMMAR_SRC_DIR=$PROJECT_SRC_DIR/$GRAMMAR
fi

if ! [[ -f "$FILE" ]]; then
  echo "No existing source file indicated."
  usage_info
  exit 1
fi

if [[ -z $ANTLR_JAVA_LIB ]]; then
    echo "No library provided for ANTLR."
    antlr_lib_info
    exit 1
else
  echo "Loading ANTLR library $ANTLR_JAVA_LIB ..."
  export CLASSPATH=$ANTLR_JAVA_LIB:$CLASSPATH
fi

if [[ "${SKIP_BUILD}" = false ]]; then
  echo "Regenerating ANTLR Lexer/Parser files ..."
  java org.antlr.v4.Tool -no-listener -visitor -o $ANTLR_OUTPUT_DIRECTORY $GRAMMAR_SRC_DIR/${grammar_source_files[$GRAMMAR]}
  echo "Compiling Java sources ..."
  javac -d $ANTLR_OUTPUT_DIRECTORY $ANTLR_OUTPUT_DIRECTORY/*.java
else
  echo "Will skip build."
fi

export CLASSPATH=$ANTLR_OUTPUT_DIRECTORY:$CLASSPATH
java org.antlr.v4.gui.TestRig ${grammar_names[$GRAMMAR]} ${grammar_start_rules[$GRAMMAR]} -gui -tree $FILE
