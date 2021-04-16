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
    Cpp)
        do_not_do_list=" \
kirikiri-tjs \
clif \
        "
        ;;

    CSharp)
        do_not_do_list="
_grammar-test \
apex \
arithmetic \
asm/masm \
asn/asn_3gpp \
calculator \
clif \
cpp \
csharp \
dice \
fortran77 \
haskell \
html \
hypertalk \
idl \
java/java9 \
javadoc \
javascript/ecmascript \
javascript/jsx \
kirikiri-tjs \
kotlin/kotlin \
logo/ucb-logo \
lpc \
objc \
pgn \
php \
pike \
powerbuilder \
python/python2 \
python/python2-js \
python/python3 \
python/python3-js \
python/python3-py \
python/python3-ts \
python/python3-without-actions \
python/python3alt \
python/tiny-python \
rego \
rexx \
sql/hive \
sql/plsql \
sql/sqlite \
sql/tsql \
stringtemplate \
swift-fin \
swift/swift2 \
swift/swift3 \
tcpheader \
thrift \
unicode/unicode16 \
v \
wat \
xpath/xpath31 \
z \
        "
	todo_pattern="^(?!.*(`echo $do_not_do_list | sed 's/\n\r/ /' | sed 's/  / /g' | sed 's/ /|/g'`)/\$)"
	echo $todo_pattern
        ;;

    Dart)
        do_not_do_list="
_grammar-test \
antlr/antlr2 \
antlr/antlr3 \
antlr/antlr4 \
apex \
asm/masm \
asn/asn_3gpp \
atl \
clif \
csharp \
dart2 \
dice \
edif300 \
edn \
erlang \
focal \
fortran77 \
fusion-tables \
gff3 \
golang \
haskell \
html \
hypertalk \
icalendar \
idl \
infosapient \
java/java9 \
javadoc \
javascript/ecmascript \
javascript/javascript \
javascript/jsx \
javascript/typescript \
kirikiri-tjs \
kotlin/kotlin \
kotlin/kotlin-formal \
lambda \
logo/logo \
logo/ucb-logo \
lpc \
metric \
modula2pim4 \
molecule \
oberon \
pascal \
pdn \
pgn \
php \
powerbuilder \
prolog \
promql \
protobuf3 \
python/python2 \
python/python3 \
python/python3-js \
python/python3-py \
python/python3-ts \
python/python3-without-actions \
python/python3alt \
python/tiny-python \
quakemap \
rego \
restructuredtext \
rexx \
ruby \
rust \
sql/hive \
sql/plsql \
sql/sqlite \
sql/tsql \
stringtemplate \
swift-fin \
swift/swift2 \
swift/swift3 \
tcpheader \
terraform \
thrift \
tinymud \
v \
vb6 \
wat \
webidl \
xpath/xpath31 \
z \
        "
	todo_pattern="^(?!.*(`echo $do_not_do_list | sed 's/\n\r/ /' | sed 's/  / /g' | sed 's/ /|/g'`)/\$)"
	echo $todo_pattern
        ;;

    Go)
        do_not_do_list=" \
_grammar-test \
algol60 \
antlr/antlr2 \
antlr/antlr3 \
antlr/antlr4 \
apex \
arithmetic \
asm/asmMASM \
asm/asmZ80 \
asm/masm \
asm/pdp7 \
asn/asn_3gpp \
clif \
cpp \
csharp \
dice \
edif300 \
edn \
erlang \
flatbuffers \
focal \
fortran77 \
gff3 \
golang \
guitartab \
haskell \
html \
hypertalk \
icalendar \
inf \
informix \
java/java \
java/java8 \
java/java9 \
javadoc \
javascript/ecmascript \
javascript/javascript \
javascript/jsx \
javascript/typescript \
joss \
kirikiri-tjs \
kotlin/kotlin \
kotlin/kotlin-formal \
kuka \
lambda \
logo/logo \
logo/ucb-logo \
lolcode \
lpc \
lua \
mckeeman-form \
metric \
modula2pim4 \
molecule \
moo \
muddb \
oberon \
oncrpc \
p \
pascal \
pddl \
pdn \
pgn \
php \
ply \
powerbuilder \
protobuf3 \
python/python2 \
python/python3 \
python/python3-js \
python/python3-py \
python/python3-ts \
python/python3-without-actions \
python/python3alt \
python/tiny-python \
quakemap \
r \
rego \
rexx \
rust \
scala \
scss \
sgf \
sieve \
smalltalk \
smtlibv2 \
sparql \
sql/hive \
sql/plsql \
sql/sqlite \
sql/tsql \
stringtemplate \
swift-fin \
swift/swift2 \
swift/swift3 \
tcpheader \
terraform \
thrift \
tinymud \
toml \
trac \
ttm \
turing \
turtle \
turtle-doc \
url \
v \
vb6 \
vba \
verilog/verilog \
wat \
webidl \
xpath/xpath1 \
xpath/xpath31 \
z \
        "
	todo_pattern="^(?!.*(`echo $do_not_do_list | sed 's/\n\r/ /' | sed 's/  / /g' | sed 's/ /|/g'`)/\$)"
	echo $todo_pattern
        ;;

    Java)
        do_not_do_list=" \
_grammar-test \
clif \
dice \
html \
hypertalk \
javascript/ecmascript \
kotlin/kotlin \
kotlin/kotlin-formal \
lambda \
less \
metric \
molecule \
p \
php \
powerbuilder \
python/python2 \
python/python3-js \
python/python3-py \
python/python3-ts \
python/python3-without-actions \
python/tiny-python \
scss \
sql/hive \
sql/plsql \
sql/tsql \
stringtemplate \
swift-fin \
swift/swift2 \
tcpheader \
wat \
        "
	todo_pattern="^(?!.*(`echo $do_not_do_list | sed 's/\n\r/ /' | sed 's/  / /g' | sed 's/ /|/g'`)/\$)"
	echo $todo_pattern
        ;;

    JavaScript)
        do_not_do_list=" \
_grammar-test \
antlr/antlr2 \
apex \
arithmetic \
asm/masm \
asn/asn_3gpp \
basic \
c \
clif \
cql3 \
csharp \
dice \
fortran77 \
golang \
haskell \
html \
hypertalk \
idl \
java/java \
java/java9 \
javadoc \
javascript/ecmascript \
javascript/javascript \
javascript/jsx \
javascript/typescript \
kotlin/kotlin \
lambda \
less \
logo/ucb-logo \
lua \
matlab \
metric \
molecule \
p \
pcre \
pgn \
php \
pike \
powerbuilder \
promql \
python/python2 \
python/python3 \
python/python3-js \
python/python3-py \
python/python3-ts \
python/python3-without-actions \
python/tiny-python \
rego \
rexx \
ruby \
rust \
sql/hive \
sql/plsql \
sql/tsql \
stringtemplate \
swift-fin \
swift/swift2 \
swift/swift3 \
tcpheader \
thrift \
v \
wat \
xpath/xpath31 \
z \
"
	todo_pattern="^(?!.*(`echo $do_not_do_list | sed 's/\n\r/ /' | sed 's/  / /g' | sed 's/ /|/g'`)/\$)"
	echo $todo_pattern
	;;

    Python3)
        do_not_do_list=" \
_grammar-test \
antlr/antlr2 \
antlr/antlr3 \
antlr/antlr4 \
apex \
asm/asmMASM \
asm/masm \
asn/asn_3gpp \
clif \
cpp \
cql3 \
csharp \
css3 \
dice \
dot \
edif300 \
edn \
erlang \
flatbuffers \
focal \
fortran77 \
gff3 \
gml \
golang \
haskell \
html \
hypertalk \
icalendar \
idl \
informix \
infosapient \
java/java9 \
javadoc \
javascript/ecmascript \
javascript/javascript \
javascript/jsx \
javascript/typescript \
joss \
kotlin/kotlin \
kotlin/kotlin-formal \
kuka \
lambda \
less \
logo/logo \
logo/ucb-logo \
lolcode \
mckeeman-form \
mdx \
metric \
microc \
modula2pim4 \
molecule \
moo \
muddb \
mumath \
oberon \
p \
parkingsign \
pascal \
pddl \
peoplecode \
pgn \
php \
pl0 \
ply \
powerbuilder \
protobuf3 \
python/python2 \
python/python3 \
python/python3-js \
python/python3-py \
python/python3-ts \
python/python3-without-actions \
python/python3alt \
python/tiny-python \
quakemap \
redcode \
rego \
rexx \
rfc1960 \
ruby \
rust \
scala \
scss \
sexpression \
sgf \
smalltalk \
sparql \
sql/hive \
sql/plsql \
sql/sqlite \
sql/tsql \
stellaris \
stringtemplate \
swift-fin \
swift/swift2 \
swift/swift3 \
tcpheader \
terraform \
tinyc \
tinymud \
toml \
turing \
turtle \
turtle-doc \
v \
vb6 \
vba \
wat \
wavefront \
webidl \
xpath/xpath31 \
z \
        "
	todo_pattern="^(?!.*(`echo $do_not_do_list | sed 's/\n\r/ /' | sed 's/  / /g' | sed 's/ /|/g'`)/\$)"
	echo $todo_pattern
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
	dotnet tool uninstall -g trgen
	dotnet tool install -g trgen --version 0.5.3
	dotnet tool uninstall -g trxml2
	dotnet tool install -g trxml2 --version 0.5.0
	# 1) Generate driver source code from poms.
	rm -rf `find . -name Generated -type d`
	echo "Generating drivers."
	bad=`trgen --todo-pattern "$todo_pattern" -t "$target" --template-sources-directory _scripts/templates/ --antlr-tool-path /tmp/antlr-4.9.2-complete.jar`
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
