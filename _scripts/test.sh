#!/usr/bin/bash

# true or false
quiet=true

# Get full path of this script.
full_path_script=$(realpath $0)
full_path_templates=$(dirname $full_path_script)/templates

# Sanity checks for required environment.
unameOut="$(uname -s)"
case "${unameOut}" in
    Linux*)     machine=Linux;;
    Darwin*)    machine=Mac;;
    CYGWIN*)    machine=Cygwin;;
    MINGW*)     machine=MinGw;;
    *)          machine="UNKNOWN:${unameOut}"
esac
if [[ "$machine" != "Linux" && "$machine" != "Mac" && "$machine" != "MinGw" ]]
then
    echo "This script runs only in a Linux environment. Skipping."
    exit 0
fi

setupdeps()
{
    trgen --version
    if [ $? != "0" ]
    then
        echo "Setting up trgen and antlr jar."
        dotnet tool install -g trgen --version 0.19.0
        dotnet tool install -g triconv --version 0.19.0
        dotnet tool install -g trxml2 --version 0.19.0
        dotnet tool install -g trwdog --version 0.19.0
    case "${unameOut}" in
        Linux*)     curl 'https://repo1.maven.org/maven2/org/antlr/antlr4/4.11.1/antlr4-4.11.1-complete.jar' -o $antlr4jar ;;
        Darwin*)    curl 'https://repo1.maven.org/maven2/org/antlr/antlr4/4.11.1/antlr4-4.11.1-complete.jar' -o $antlr4jar ;;
        CYGWIN*)    curl 'https://repo1.maven.org/maven2/org/antlr/antlr4/4.11.1/antlr4-4.11.1-complete.jar' -o $antlr4jar ;;
        MINGW*)     curl 'https://repo1.maven.org/maven2/org/antlr/antlr4/4.11.1/antlr4-4.11.1-complete.jar' -o $antlr4jar ;;
        *)          echo 'unknown machine'
    esac
        echo "Done setting up."
    fi
}

thetime()
{
    local hh
    local DIFF
    DIFF=$1
    hh="$(($DIFF / 3600 ))"
    mm="$((($DIFF % 3600) / 60))"
    ss="$(($DIFF % 60))"
    printf "%02d:%02d:%02d" $hh $mm $ss
}

function getopts-extra () {
    OPTARG=()
    declare i=0
    # if the next argument is not an option, then append it to array OPTARG
    while [[ ${OPTIND} -le $# && ${!OPTIND:0:1} != '-' ]]; do
        OPTARG[i]=${!OPTIND}
        let i++ OPTIND++
    done
}

# filter="all" or "diff" or "agnostic"
filter="all"
# order="grammar" or "target"
order="grammar"
failed=()
succeeded=()
skipped=""
grammars=()
targets=()
tests=()

# Get "root" of the repo clone.
cwd=`pwd`
prefix=`pwd`
pushd $prefix
while true
do
	if [ -f `pwd`/_scripts/test.sh ]
	then
		break
	elif [ `pwd` == "/" ]
	then
		break
	fi
	cd ..
done
if [ ! -f `pwd`/_scripts/test.sh ]
then
	echo "Not in repo."
	exit 1
fi
prefix=`pwd`
popd

rm -rf `find . -name 'Generated*' -type d`

# Parse args, and update computation to perform.
order="grammars"
additional=()
antlr4jar=/tmp/antlr4-complete.jar
while getopts 'agthf' opt; do
    case "$opt" in
        a)
            getopts-extra "$@"
            antlr4jar="${OPTARG[0]}"
	    echo Antlr4jar is $antlr4jar
            ;;
        g)
            getopts-extra "$@"
            for a in "${OPTARG[@]}"
            do
                grammars+=( "$a" )
            done
            ;;
        t)
            getopts-extra "$@"
            for a in "${OPTARG[@]}"
            do
                targets+=( "$a" )
            done
            ;;
        o)
            order="$OPTARG"
            ;;
        f)
            getopts-extra "$@"
            filter="${OPTARG[0]}"
            for a in "${OPTARG[@]:1}"
            do
                additional+=( "$a" )
            done
            ;;
        ?|h)
            cat - <<EOF
NAME
       test - tests Antlr4 grammars

SYNOPSIS
       $(basename $0) ([-g ...] | [-s ...] | [-t ...])

DESCRIPTION
       Tests Antlr4 grammars. Each grammar requires pom.xml and desc.xml files.

OPTIONS
       -g
           Specifies a list of directories that are searched for grammars.
           A grammar requires pom.xml and desc.xml files for it to be tested.
           If the option is not specified, then the default is to search from
           the current directory. Searching is subject to filters.

       -f
           Specifies the type of testing to filter. Possible types are "diff",
		   "all", or "agnostic". The default is "all".

       -t
           Specifies the targets to test. Possible targets are "Antlr4cs",
           "CSharp", "Cpp", "Dart", "Go", "Java", "JavaScript", "PHP", "Python3".
		   All targets are tested by default.

EOF
            exit 0
            ;;
    esac
done

echo $filter $first $second

if [ "$filter" == "diff" ]
then
    if [ "${#additional[@]}" -eq 0 ]
    then
        diffs=`git diff --exit-code --name-only .`
        if [ "$diffs" != "" ]
        then
            grammars=()
            # Test grammars for the enclosing directories.
            directories=`git diff --name-only . 2> /dev/null | sed 's#\(.*\)[/][^/]*$#\1#' | sort -u | grep -v _scripts`
            for g in $directories
            do
				pushd $g
				echo here
				while true
				do
				    if [ -f `pwd`/desc.xml ]
				    then
				        break
					elif [ `pwd` == "$prefix" ]
					then
						break
					fi
					cd ..
			    done
				g=`pwd`
		        g=${g##*$prefix/}
				popd
                echo Adding diff $g
                grammars+=( $g )
            done
			grammars=( $(for g in "${grammars[@]}"; do echo "${g}"; done | sort -u) )
            echo Grammars to do are: ${grammars[@]}
        fi
    elif [ "${#additional[@]}" -eq 2 ]
    then
            grammars=()
            # Test grammars for the enclosing directories.
            directories=`git diff --name-only ${additional[0]} ${additional[1]} 2> /dev/null | sed 's#\(.*\)[/][^/]*$#\1#' | sort -u | grep -v _scripts`
            for g in $directories
            do
                echo Adding diff $g
                grammars+=( $g )
            done
            echo Grammars to do are: ${grammars[@]}
    else
        echo ouch
        exit 1
    fi
elif [ "$filter" != "all" ]
then
    echo Unknown filter $filter
    exit 1
fi

if [ "$grammars" == "" ]
then
echo gramar is dot
    grammars=( "." )
fi
new_grammars=()
for d in ${grammars[@]}
do
    if [ ! -d "$d" ]
    then
        continue
    fi
    desc=`find $d -name desc.xml`
    for i in $desc
    do
        if [ "$i" == "desc.xml" ]; then i='.'; fi
        directory=${i%/*}
        pushd $directory > /dev/null
        directory=${directory##*$prefix/}
        testname=$directory
        new_grammars+=( "$testname" )
        popd > /dev/null
    done
done
grammars=("${new_grammars[@]}")

if [ "$targets" == "" ]
then
    targets=( Antlr4cs CSharp Cpp Dart Go Java JavaScript PHP Python3 )
fi

echo grammars = ${grammars[@]}
echo targets = ${targets[@]}
echo order = $order
echo filter = $filter

# Compute cross product
for g in ${grammars[@]}
do
    for t in ${targets[@]}
    do
        tests+=( "$g,$t" )
    done
done

# Sort the list of <grammar, target> tuples if needed.
if [[ "$order" == "grammars" ]]; then sorted="1"; else sorted="2"; fi
#tests=`echo $tests | fmt -w 1 | sort -t ',' -k "$sorted"`

echo Tests now ${tests[@]}

# Perform tests in order.
for test in ${tests[@]}
do
    all=( $(echo $test | tr "," "\n") )
    testname=${all[0]}
    target=${all[1]}
    pushd $testname > /dev/null
    if [ ! -f desc.xml ]
    then
        echo No desc.xml for $testname
        popd > /dev/null
        continue
    fi
    desc_targets=`trxml2 desc.xml | grep '/desc/targets'`
    desc_targets="${desc_targets##*=}"
    desc_targets=`echo "$desc_targets" | tr ',' ' ' | tr ';' ' '`

    yes=false;
    for t in $desc_targets
    do
        if [ "$t" == "+all" ]; then yes=true; fi
        if [ "$t" == "-$target" ]; then yes=false; fi
        if [ "$t" == "$target" ]; then yes=true; fi
    done

    if [ "$yes" == "false" ]
    then
        echo Skipping $test because target $target does not work for it.
        popd > /dev/null
        continue
    fi

    if [ "$filter" == "agnostic" ]
    then
        # Test whether the grammars have actions.
        count=`trparse -t antlr4 *.g4 2> /dev/null | trxgrep ' //(actionBlock | argActionBlock)' | trtext -c`
        if [ "$count" == "0" ]
        then
            echo "no actions => skipping $testname."
            popd > /dev/null
            continue
        fi
    fi

    # Generate driver source code.
    echo -n "$testname,$target:"

    if [ $quiet != "true" ]; then echo "Generating driver for $testname."; fi
    bad=`trgen -t "$target" --template-sources-directory "$full_path_templates" --antlr-tool-path $antlr4jar 2> /dev/null`
    for i in $bad; do failed+=( "$testname/$target" ); done

    if [ ! -f Generated-$target/build.sh ]; then echo " no build.sh"; popd > /dev/null 2>&1; continue; fi

    # Build driver code.
    if [ $quiet != "true" ]; then echo "Building."; fi
    pushd Generated-$target > /dev/null
    date1=$(date +"%s")
    bash build.sh > output.txt 2>&1
    status="$?"
    date2=$(date +"%s")
    DIFF=$(($date2-$date1))
    length=" Build "`thetime $DIFF`
    echo -n " $length"
    if [ "$status" != "0" ]; then echo ""; cat output.txt; fi
    if [[ "$status" != "0" ]]
    then
        echo " Failed."
        failed+=( "$testname/$target" )
        popd > /dev/null
        popd > /dev/null
        continue
    fi

    # Test generated parser on examples.
    if [ $quiet != "true" ]; then echo "Parsing."; fi
    date1=$(date +"%s")
    bash test.sh > output.txt 2>&1
    status="$?"
    date2=$(date +"%s")
    DIFF=$(($date2-$date1))
    length=" Test "`thetime $DIFF`
    echo -n " $length"
    if [ "$status" != "0" ]; then echo ""; cat output.txt; fi
    if [[ "$status" != "0" ]]
    then
        echo " Failed."
        failed+=( "$testname,$target" )
    else
        echo " Succeeded."      
        succeeded+=( "$testname,$target" )
    fi
    popd > /dev/null
    popd > /dev/null
done

if [[ "$failed" == "" ]]
then
    exit 0
else
    exit 1
fi
