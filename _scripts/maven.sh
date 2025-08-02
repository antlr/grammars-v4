#!/bin/sh
#set -x
#set -e

grammars=()
directories=`git diff --name-only $1 $2 -- . 2> /dev/null | sed 's#\(.*\)[/][^/]*$#\1#' | sort -u | grep -v _scripts | grep -v .github | grep -v '^[.]$' | grep -v target | grep -v Generated`
prefix=`pwd`
for g in $directories
do
    pushd $g
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
        popd > /dev/null
        continue
    fi
    grep -q -e '<modules>' pom.xml
    if [ $? -eq 0 ]
    then
        popd > /dev/null
        continue
    fi
    popd > /dev/null
    grammars+=( $g )
done

for grammar in ${grammars[@]}
do
    pushd $grammar
    mvn -B package --file pom.xml
    popd
done
