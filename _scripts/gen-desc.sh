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
        Linux*)     curl 'https://repo1.maven.org/maven2/org/antlr/antlr4/4.11.1/antlr4-4.11.1-complete.jar' -o /tmp/antlr4-4.11.1-complete.jar;;
        Darwin*)    curl 'https://repo1.maven.org/maven2/org/antlr/antlr4/4.11.1/antlr4-4.11.1-complete.jar' -o /tmp/antlr4-4.11.1-complete.jar;;
        CYGWIN*)    curl 'https://repo1.maven.org/maven2/org/antlr/antlr4/4.11.1/antlr4-4.11.1-complete.jar' -o /tmp/antlr4-4.11.1-complete.jar;;
        MINGW*)     curl 'https://repo1.maven.org/maven2/org/antlr/antlr4/4.11.1/antlr4-4.11.1-complete.jar' -o /tmp/antlr4-4.11.1-complete.jar;;
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

# strategy="actions" or "git"
strategy=""
failed=""
succeeded=""
skipped=""
grammars=""
targets=""
tests=""
rm -rf `find . -name 'Generated*' -type d`

# Parse args, and update computation to perform.
order="grammars"
while getopts 'g:t:h' opt; do
  case "$opt" in
    g)
      order="grammars"
      arg="$OPTARG"
      for a in $arg
      do
          grammars=`add "$grammars" "$a"`
      done
      ;;

    t)
      order="targets"
      arg="$OPTARG"
      for a in $arg
      do
          targets=`add "$targets" "$a"`
      done
      ;;

    ?|h)
      echo "Usage: $(basename $0) [-a] [-b] [-c arg]"
      exit 1
      ;;
  esac
done

if [ "$grammars" == "" ]
then
    poms=`find $prefix -name pom.xml`
    for i in $poms
    do
        i=${i##*$prefix/}
        if [ "$i" == "pom.xml" ]; then i='.'; fi
        directory=${i%/*}
        echo Starting analysis of $directory
        pushd $directory > /dev/null
        # determine whether the pom.xml has grammar description.
        has_goal=`trxml2 pom.xml | grep '/executions/execution/goals/goal'`
        if [ "$has_goal" == "" ]
        then
            popd > /dev/null
            continue
        fi
        testname=$directory
        grammars=`add "$grammars" "$testname"`
        popd > /dev/null
    done
fi

echo "Evaluating targets for each grammar..."

prefix=`pwd`
targets="Antlr4cs CSharp Cpp Dart Go Java JavaScript PHP Python3"
for i in $grammars
do
    directory=$i
    echo Starting analysis of $directory
    pushd $directory > /dev/null

    # determine whether the pom.xml has grammar description.
    has_goal=`trxml2 pom.xml | grep '/executions/execution/goals/goal'`
    if [ "$has_goal" == "" ]
    then
        popd > /dev/null
        continue
    fi
    testname=$directory

    desc=""
    
    for t in $targets
    do

        target=$t

        # Generate driver source code.
        echo -n "$testname,$target:"

        bad=`trgen -t "$target" --template-sources-directory "$full_path_templates" --antlr-tool-path /tmp/antlr4-4.11.1-complete.jar 2> /dev/null`
        for i in $bad; do failed=`add "$failed" "$testname/$target"`; done

        if [ ! -f Generated-$target/build.sh ]
        then
            continue
        fi

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
        if [[ "$status" != "0" ]]
        then
            echo " Failed."
            popd > /dev/null
            continue
        fi
        echo " Succeeded."
        if [ "$desc" == "" ]; then desc="+$target"; else desc="$desc;+$target"; fi
        popd > /dev/null
    done

    echo Target description for $g is $desc
    cat - <<EOF > desc.xml
<desc>
   <targets>$desc</targets>
</desc>
EOF

    popd > /dev/null
done
