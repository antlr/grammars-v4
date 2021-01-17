#!/bin/bash

# CSharp grammar regression tests.
#
# This script tests Antlr grammars by generating a driver, compiling,
# it and running the generate parse on input. Most grammars are target
# independent, i.e., they do not include actions. These grammars
# compile and parse fine. However, there are about a third which
# currently are written for a particular target. These grammars are
# manually removed from testing with the "do_not_do_list". If you
# adjust the variable value (see the code below), then make sure there
# is a space after each test name, before the backslash.

do_not_do_list=" \
idl \
iri \
molecule \
rfc1035 \
rfc1960 \
tcpheader \
unicode/graphemes \
abb \
======== \
antlr/antlr2 \
antlr/antlr3 \
antlr/antlr4 \
apex \
asm/masm \
asn/asn_3gpp \
cobol85 \
cpp \
cql3 \
csharp \
cto \
dice \
fortran77 \
fusion-tables \
graphstream-dgs \
haskell \
html \
java/java \
java/java8 \
java/java9 \
javadoc \
javascript/ecmascript \
kirikiri-tjs \
kotlin/kotlin-formal \
less \
logo/ucb-logo \
lpc \
mckeeman-form \
muparser \
oncrpc \
pgn \
php \
powerbuilder \
promql \
prov-n \
python/python3 \
python/python3alt \
r \
rego \
rexx \
rfc822/rfc822-datetime \
rfc822/rfc822-emailaddress \
scss \
sharc \
sql/hive \
sql/mysql \
sql/plsql \
sql/sqlite \
sql/tsql \
stringtemplate \
swift/swift2 \
swift/swift3 \
swift-fin \
thrift \
tnsnames \
turtle-doc \
v \
vb6 \
wat \
xml \
xpath/xpath31 \
xsd-regex \
z \
"
echo "These grammars will not be tested for the CSharp target:"
echo $do_not_do_list | fmt

failed=""
succeeded=""
skipped=""

# This function outputs "yes" if "foo" exists in $list, otherwise "no".
contains() {
    if [[ " $1 " =~ " $2 " ]]
    then
        echo yes
    else
        echo no
    fi
}

# This function sets the variable "failed" with the give named test.
# Duplicates are avoided.
update_failed() {
    if [[ `contains "$failed" "$1"` == "no" ]]
    then
        failed="$failed $1"
    fi
}

# This function sets the variable "succeeded" with the give named test.
# Duplicates are avoided.
update_succeeded() {
    if [[ `contains "$succeeded" "$1"` == "no" ]]
    then
        succeeded="$succeeded $1"
    fi
}

# This function sets the variable "skipped" with the give named test.
# Duplicates are avoided.
update_skipped() {
    if [[ `contains "$skipped" "$1"` == "no" ]]
    then
        skipped="$skipped $1"
    fi
}

prefix=`pwd`
prefix="$prefix"/

# Uncomment this line to remove the temporary directories.
# rm -rf `find . -name Generated`

recurse()
{
    # In the current working directory, extract sub-modules from the pom.xml.
    d=`cat pom.xml | xml2 | grep modules/module= | sed 's/=/ /' | awk '{print $2}'`
    if [[ "$d" == "" ]]
    then
        x=`pwd`
        testname=${x#"$prefix"}
        echo "===================================================="
        echo Testing CSharp parsing for $testname
        echo ""
        filtered=`contains "$do_not_do_list" "$testname"`
        s=`cat pom.xml | xml2 | grep configuration/entryPoint= | sed 's/=/ /' | awk '{print $2}'`
        if [[ "$filtered" == "yes" ]]
        then
            echo "On do_not_do_list -- skipping."
            update_skipped "$testname"
        elif [[ "$s" == "" ]]
        then
            echo "No entry point for grammar specified in pom.xml -- skipping."
            update_skipped "$testname"
        else
            rm -rf Generated
            echo dotnet /mnt/c/Users/kenne/Documents/Antlr4BuildTasks/dotnet-antlr/bin/Debug/net5.0/dotnet_antlr.dll -s $s
            dotnet /mnt/c/Users/kenne/Documents/Antlr4BuildTasks/dotnet-antlr/bin/Debug/net5.0/dotnet_antlr.dll -s $s
            dotnet build Generated/Test.csproj
            if [[ "$?" != "0" ]]
            then
                update_failed "$testname"
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
                            update_failed "$testname"
                        fi
                    else
                        if [[ "$pass" -ne "0" ]]
                        then
                            update_failed "$testname"
                        fi
                    fi
                done
            fi
            if [[ `contains "$failed" "$testname"` == "no" ]]
            then
                update_succeeded "$testname"
            fi
            rm -rf Generated
        fi
    else
        for i in $d
        do
            cd $i
            recurse
            cd ..
        done
    fi
}

recurse

echo "Grammars that succeeded: $succeeded" | fmt
echo "Grammars that failed: $failed" | fmt
echo "Grammars skipped: $skipped" | fmt

if [[ "$failed" == "" ]]
then
    exit 0
else
    exit 1
fi
