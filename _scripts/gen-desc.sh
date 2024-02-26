#!/usr/bin/bash

# true or false
quiet=true

# Get full path of this script, and root.
full_path_script=$(realpath $0)
full_path_script_dir=`dirname $full_path_script`
full_path_templates=$(dirname $full_path_script)/templates
pushd $full_path_script_dir
cd ..
root=`pwd`
popd

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
    dotnet trgen -- --version
    if [ $? != "0" ]
    then
        echo "Need to set up Trash."
        exit 0
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
    descs=`find . -name desc.xml`
    for i in $descs
    do
        echo "i = $i"
        d=`dirname $i`
        pushd $d > /dev/null
        d=`pwd`
        g=${d##*$root/}
        testname=$g
        grammars=`add "$grammars" "$testname"`
        popd > /dev/null
    done
fi

echo "grammars = $grammars"
echo "Evaluating targets for each grammar..."

targets="CSharp Cpp Dart Go Java JavaScript PHP Python3 TypeScript"
for g in $grammars
do
    echo Starting analysis of $g
    cd $root/$g
    desc=""
    for t in $targets
    do
        cd $root/$g
        rm -rf Generated-*
        echo "$g,$t:"
        dotnet trgen -- -t "$t" --template-sources-directory "$full_path_templates"
        if [ "$?" -ne 0 ]
        then
            failed=`add "$failed" "$g/$t"`
            continue
        fi
        for j in Generated-*
        do
            cd $root/$g/$j
            echo "Build"
            date1=$(date +"%s")
            bash build.sh
            status="$?"
            date2=$(date +"%s")
            DIFF=$(($date2-$date1))
            length=`thetime $DIFF`
            echo "$length "
            if [ "$status" -ne 0 ]
            then
                echo Fail.
                continue
            fi
            echo "Test "
            date1=$(date +"%s")
            bash test.sh
            status="$?"
            date2=$(date +"%s")
            DIFF=$(($date2-$date1))
            length=`thetime $DIFF`
            echo "$length "
            if [ "$status" != "0" ]
            then
                echo Fail.
                continue
            fi
            echo Success.
            if [ "$desc" == "" ]; then desc="$t"; else desc="$desc;$t"; fi
        done
        rm -rf Generated-*
    done
    echo Target description for $g is $desc
	cd $root/$g
    cat - <<EOF > desc2.xml
<desc>
   <targets>$desc</targets>
</desc>
EOF
	cat desc2.xml
done
