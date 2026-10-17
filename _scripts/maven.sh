#!/usr/bin/bash
#set -x
#set -e

grammars=()
if [ $# -eq 0 ]; then
    directories=`find . -name pom.xml | sed 's#\(.*\)[/][^/]*$#\1#' | sort -u | grep -v _scripts | grep -v .github | grep -v '^[.]$' | grep -v target | grep -v Generated`
else
    directories=`git diff --name-only $1 $2 -- . 2> /dev/null | sed 's#\(.*\)[/][^/]*$#\1#' | sort -u | grep -v _scripts | grep -v .github | grep -v '^[.]$' | grep -v target | grep -v Generated`
fi
prefix=`pwd`
for g in $directories
do
    pushd $g > /dev/null 2>&1
    while true
    do
        if [ `pwd` == '/' ]
        then
            break
        elif [ -f pom.xml ]
        then
            break
        elif [ `pwd` == "$prefix" ]
        then
            break
        fi
        cd ..
    done
    g=`pwd`
    g=${g##*$prefix}
    g=${g##/}
    if [ "$g" == "" ]
    then
        g="."
    fi
    if [ ! -f pom.xml ]
    then
        popd > /dev/null 2>&1
        continue
    fi
    grep -q -e '<modules>' pom.xml
    if [ $? -eq 0 ]
    then
        popd > /dev/null 2>&1
        continue
    fi
    popd > /dev/null 2>&1
    if [[ ! " ${grammars[@]} " =~ " $g " ]]; then
        grammars+=( $g )
    fi
done

echo All grammars:
echo ${grammars[@]}

failed_grammars=()
for grammar in ${grammars[@]}
do
    pushd $grammar
    mvn -B clean test
    if [ $? -ne 0 ]; then
        failed_grammars+=( $grammar )
    fi
    popd
done

# Perform basic global integrity check.
root=`git rev-parse --show-toplevel`
pushd $root
mvn clean
status=$?

if [ ${#failed_grammars[@]} -ne 0 ]; then
    echo "The following grammars failed:"
    for g in "${failed_grammars[@]}"; do
        echo "  $g"
    done
    exit 1
fi

exit $status
