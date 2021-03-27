#!/usr/bin/bash

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
# idl iri molecule rfc1035 rfc1960 tcpheader unicode/graphemes abb 

target="$1"
if [[ "$1" == "" ]]
then
    target="CSharp"
fi
case "$target" in
    CSharp)
        do_not_do_list=" \
_grammar-test acme apex apt \
arithmetic asm/masm asn/asn_3gpp atl \
basic \
calculator capnproto cpp csharp cto \
dcm dgol dice \
erlang \
fortran77 \
graphql gtin \
haskell html http hypertalk \
idl infosapient \
java/java9 javadoc javascript/ecmascript javascript/jsx joss \
kirikiri-tjs kotlin/kotlin \
logo/logo logo/ucb-logo lpc \
molecule morsecode \
objc \
pddl pgn php pike pmmn powerbuilder python/python2 python/python2-js \
python/python3 python/python3-js python/python3-py python/python3-ts \
python/python3-without-actions python/python3alt python/tiny-python \
rcs rego restructuredtext rexx rfc1035 rfc1960 rfc3080 \
sharc smiles sql/hive sql/mysql sql/plsql sql/sqlite sql/tsql \
stacktrace stringtemplate swift-fin swift/swift2 swift/swift3 \
tcpheader terraform thrift \
unicode/unicode16 \
v \
wat \
xpath/xpath31 \
z \
        "
        ;;

    Java)
        do_not_do_list=" \
        "
        ;;

    JavaScript)
        do_not_do_list=" \
        "
        ;;

    Cpp)
        do_not_do_list=" \
kirikiri-tjs \
        "
        ;;

    *)          echo "Unknown target"; exit 1;;
esac
do_not_do_list=`echo $do_not_do_list | sed 's/^ //g' | sed 's/ /,/g'`

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
if [[ "$machine" != "Linux" && "$machine" != "Mac" && "$machine" != "MinGw" ]]
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

rm -rf `find . -name Generated -type d`

build()
{
    local x="$1"
    local testname
    testname="$x"
    echo "===================================================="
    echo Building $testname
    echo ""
    # Assert(cwd is valid)
    pushd "$x/Generated"
    if [[ $? != "0" ]]
    then
        echo "$1 is not a valid directory"
        exit 1
    fi
    date1=$(date +"%s")
    make
    status="$?"
    date2=$(date +"%s")
    DIFF=$(($date2-$date1))
    echo "Duration: $(($DIFF / 3600 )) hours $((($DIFF % 3600) / 60)) minutes $(($DIFF % 60)) seconds"
    if [[ "$status" != "0" ]]
    then
        failed=`add "$failed" "$testname"`
    fi
    popd
}

test()
{
    local x="$1"
    local testname
    testname="$x"
    echo "===================================================="
    echo Testing $testname
    echo ""
    # Assert(cwd is valid)
    pushd "$x/Generated"
    if [[ $? != "0" ]]
    then
        echo "$1 is not a valid directory"
        exit 1
    fi
    date1=$(date +"%s")
    make test
    status="$?"
    date2=$(date +"%s")
    DIFF=$(($date2-$date1))
    echo "Duration: $(($DIFF / 3600 )) hours $((($DIFF % 3600) / 60)) minutes $(($DIFF % 60)) seconds"
    if [[ "$status" != "0" ]]
    then
        failed=`add "$failed" "$testname"`
    else
        succeeded=`add "$succeeded" "$testname"`
    fi
    popd
}

# Main
# 0) Set up.
part1()
{
	date
	dotnet tool uninstall -g dotnet-antlr
	dotnet tool install -g dotnet-antlr --version 3.0.12
	dotnet tool uninstall -g csxml2
	dotnet tool install -g csxml2 --version 1.0.0
	# 1) Generate driver source code from poms.
	echo "These grammars will not be tested:"
	echo $do_not_do_list | fmt
	rm -rf `find . -name Generated -type d`
	echo "Generating drivers."
	bad=`dotnet-antlr -m true -k "$do_not_do_list" -t "$target" --template-sources-directory _scripts/templates/ --antlr-tool-path /tmp/antlr-4.9.2-complete.jar`
	for i in $bad; do failed=`add "$failed" "$i"`; done
	date
}

part2()
{
	# 2) Build driver code.
	echo "Building."
	date
	case "$target" in
	    CSharp) build_file_type="Test.csproj" ;;
	    *) build_file_type="makefile" ;;
	esac
	echo prefix $prefix
	echo bft $build_file_type
	build_files=`find $prefix -type f -name $build_file_type | grep Generated`
	echo bf $build_files
	for build_file in $build_files
	do
	    p1="$(dirname "${build_file}")"
	    p2=${p1#"$prefix/"}
	    testname="$(dirname "${p2}")"
	    con=`contains "$failed" "$testname"`
	    if [[ "$con" == "no" ]]
	    then
	        build "$testname"
	    fi
	done
	date
}

part3()
{
	# 3) Test generated parser on examples.
	echo "Parsing."
	date
	build_files=`find $prefix -type f -name $build_file_type | grep Generated`
	echo bf $build_files
	for build_file in $build_files
	do
	    p1="$(dirname "${build_file}")"
	    p2=${p1#"$prefix/"}
	    testname="$(dirname "${p2}")"
	    con=`contains "$failed" "$testname"`
	    if [[ "$con" == "no" ]]
	    then
	        test "$testname"
	    fi
	done
	date
}

part1
part2
part3
echo "Grammars that succeeded: $succeeded" | fmt
echo "================"
echo "Grammars that failed: $failed" | fmt
echo "================"

if [[ "$failed" == "" ]]
then
    exit 0
else
    exit 1
fi
