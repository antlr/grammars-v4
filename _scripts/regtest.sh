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
    Cpp) do_not_do_list=`cat _scripts/skip-cpp.txt` ;;
    CSharp) do_not_do_list=`cat _scripts/skip-csharp.txt` ;;
    Dart) do_not_do_list=`cat _scripts/skip-dart.txt` ;;
    Go) do_not_do_list=`cat _scripts/skip-go.txt` ;;
    Java) do_not_do_list=`cat _scripts/skip-java.txt` ;;
    JavaScript) do_not_do_list=`cat _scripts/skip-javascript.txt` ;;
    Python3) do_not_do_list=`cat _scripts/skip-python3.txt` ;;
    *) echo "Unknown target"; exit 1;;
esac
invert="$2"
if [[ "$invert" == "" ]]
then
    skip_pattern="^(.*/(`echo $do_not_do_list | sed 's/\n/ /g' | sed 's/\r/ /g' | sed 's/  / /g' | sed 's/ $//g' | sed 's/ /|/g' | sed 's/-/\\-/g'`))/\$"
    echo Skip list pattern = $skip_pattern
else
    todo_pattern="^(.*/(`echo $do_not_do_list | sed 's/\n/ /g' | sed 's/\r/ /g' | sed 's/  / /g' | sed 's/ $//g' | sed 's/ /|/g'| sed 's/-/\\-/g'`))/\$"
    echo To do list pattern = $todo_pattern
fi

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
setupdeps()
{
    date
    echo "Setting up trgen and antlr jar."
    dotnet tool uninstall -g trgen
    dotnet tool install -g trgen --version 0.13.2
    dotnet tool uninstall -g trxml2
    dotnet tool install -g trxml2 --version 0.13.2
    dotnet tool uninstall -g trwdog
    dotnet tool install -g trwdog --version 0.13.2
	case "${unameOut}" in
		Linux*)     curl 'https://repo1.maven.org/maven2/org/antlr/antlr4/4.9.3/antlr4-4.9.3-complete.jar' -o /tmp/antlr-4.9.3-complete.jar;;
		Darwin*)    curl 'https://repo1.maven.org/maven2/org/antlr/antlr4/4.9.3/antlr4-4.9.3-complete.jar' -o /tmp/antlr-4.9.3-complete.jar;;
		CYGWIN*)    curl 'https://repo1.maven.org/maven2/org/antlr/antlr4/4.9.3/antlr4-4.9.3-complete.jar' -o /tmp/antlr-4.9.3-complete.jar;;
		MINGW*)     curl 'https://repo1.maven.org/maven2/org/antlr/antlr4/4.9.3/antlr4-4.9.3-complete.jar' -o /tmp/antlr-4.9.3-complete.jar;;
		*)          echo 'unknown machine'
	esac
	ls -l /tmp/antlr4-runtime-4.9.3.jar
    echo "Done setting up."
    date
}

part1()
{
    date
    # 1) Generate driver source code from poms.
    rm -rf `find . -name Generated -type d`
    echo "Generating drivers."
    if [[ "$invert" == "" ]]
    then
        bad=`trgen --skip-pattern "$skip_pattern" -t "$target" --template-sources-directory _scripts/templates/ --antlr-tool-path /tmp/antlr-4.9.3-complete.jar`
    else
        bad=`trgen --todo-pattern "$todo_pattern" -t "$target" --template-sources-directory _scripts/templates/ --antlr-tool-path /tmp/antlr-4.9.3-complete.jar`
    fi
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

setupdeps
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
