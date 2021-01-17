#!/bin/bash

# This script tests Antlr grammars for the C# target. It searches
# pom.xml files for grammars, generates a driver, builds, and tests the
# parser on input files. The code only runs on Linux-type boxes.
# It requires Bash, rm, pwd, echo, cat, xml2, grep, sed, awk, find, and
# the NET SDK.
#
# Most grammars in this directory are target independent. However,
# there is a large number that, currently, are for a specific target.
# These grammars are skipped from testing by inclusion of the name of
# the grammar (path from the top-level directory of the repository) in
# the "do_not_do_list". If you add a new grammar with a pom.xml file
# that includes testing, then the grammar will be tested for the C#
# target. If your grammar is target specific, adjust the list
# below.

# List<string> do_not_do_list = ...;
do_not_do_list=" \
idl iri molecule rfc1035 rfc1960 tcpheader unicode/graphemes abb \
======== \
antlr/antlr2 antlr/antlr3 antlr/antlr4 apex asm/masm asn/asn_3gpp \
cobol85 cpp cql3 csharp cto dice fortran77 fusion-tables \
graphstream-dgs haskell html java/java java/java8 java/java9 \
javadoc javascript/ecmascript kirikiri-tjs kotlin/kotlin-formal \
less logo/ucb-logo lpc mckeeman-form muparser oncrpc pgn php \
powerbuilder promql prov-n python/python3 python/python3alt \
r rego rexx rfc822/rfc822-datetime rfc822/rfc822-emailaddress \
scss sharc sql/hive sql/mysql sql/plsql sql/sqlite sql/tsql \
stringtemplate swift/swift2 swift/swift3 swift-fin thrift tnsnames \
turtle-doc v vb6 wat xml xpath/xpath31 xsd-regex z \
"

# Sanity checks for required environment.
unameOut="$(uname -s)"
case "${unameOut}" in
    Linux*)     machine=Linux;;
    Darwin*)    machine=Mac;;
    CYGWIN*)    machine=Cygwin;;
    MINGW*)     machine=MinGw;;
    *)          machine="UNKNOWN:${unameOut}"
esac
echo ${machine}
if [[ "$machine" != "Linux" && "$machine" != "Mac" ]]
then
    echo "This script runs only in a Linux environment. Skipping."
    exit 0
fi

# List<string> failed = new List<string>();
failed=""

# List<string> succeeded = new List<string>();
succeeded=""

# List<string> skipped = new List<string>();
skipped=""

# string prefix = current working directory.
prefix=`pwd`

# string contains(List<string> list, string item)
# If "list" contains the "item", return "yes" else return "no".
contains() {
    if [[ " $1 " =~ " $2 " ]]
    then
        echo yes
    else
        echo no
    fi
}

# List<string> add(List<string> list, string name), without duplicates.
# This function returns a new list by adding "name" to "list".
add() {
    if [[ `contains "$1" "$2"` == "no" ]]
    then
        echo "$1 $2"
    else
        echo "$1"
    fi
}

# Uncomment this line to remove the temporary directories.
# rm -rf `find . -name Generated`

# void recurse(string cwd)
recurse()
{
    local x="$1"
    local d
    local examples
    local files
    local filtered
    local i
    local newd
    local pass
    local s
    local testname
    cd "$x"
    # Assert(cwd is valid)
    if [[ $? != "0" ]]
    then
        exit 1
    fi
    testname=${x#"$prefix/"}
    # In the current working directory, extract sub-modules from the pom.xml.
    d=`cat pom.xml | xml2 | grep modules/module= | sed 's/=/ /' | awk '{print $2}'`
    if [[ "$d" == "" ]]
    then
        echo "===================================================="
        echo Testing CSharp parsing for $testname
        echo ""
        filtered=`contains "$do_not_do_list" "$testname"`
        s=`cat pom.xml | xml2 | grep configuration/entryPoint= | sed 's/=/ /' | awk '{print $2}'`
        if [[ "$filtered" == "yes" ]]
        then
            echo "On do_not_do_list -- skipping."
            skipped=`add "$skipped" "$testname"`
        elif [[ "$s" == "" ]]
        then
            echo "No entry point for grammar specified in pom.xml -- skipping."
            skipped=`add "$skipped" "$testname"`
        else
            rm -rf Generated
            dotnet-antlr -s $s
            dotnet build Generated/Test.csproj
            if [[ "$?" != "0" ]]
            then
                failed=`add "$failed" "$testname"`
            fi
            examples=`cat pom.xml | xml2 | grep configuration/exampleFiles= | sed 's/=/ /' | awk '{print $2}'`
            if [[ "$examples" == "" ]]
            then
                echo "No examples to parse -- skipping."
            else
                files=`find "$examples" -type f | grep -v '\.tree$' | grep -v '\.errors'`
                for i in $files
                do
                    echo Parsing $i
                    cat $i | "./Generated/bin/Debug/net5.0/Test"
                    pass=$?
                    if [[ -f "${i%*}.errors" ]]
                    then
                        if [[ "$pass" -eq "1" ]]
                        then
                            echo "(Expected failed parse => OK)"
                        else
                            failed=`add "$failed" "$testname"`
                        fi
                    else
                        if [[ "$pass" -ne "0" ]]
                        then
                            failed=`add "$failed" "$testname"`
                        fi
                    fi
                done
            fi
            if [[ `contains "$failed" "$testname"` == "no" ]]
            then
                succeeded=`add "$succeeded" "$testname"`
            fi
            rm -rf Generated
        fi
    else
        for i in $d
        do
            newd="$x/$i"
            recurse "$newd"
        done
    fi
}

# Main
dotnet tool install -g dotnet-antlr
echo "These grammars will not be tested for the CSharp target:"
echo $do_not_do_list | fmt
recurse "$prefix"
echo "Grammars that succeeded: $succeeded" | fmt
echo "Grammars that failed: $failed" | fmt
echo "Grammars skipped: $skipped" | fmt
if [[ "$failed" == "" ]]
then
    exit 0
else
    exit 1
fi
