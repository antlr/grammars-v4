#!/bin/sh
#set -x
#set -e
#echo "YO. I AM HERE."

grammars=()

# Test grammars for the enclosing directories.
#echo "Files."
#git diff --name-only $1 $2 -- .
#echo "f1."
#git diff --name-only $1 $2 -- . 2> /dev/null | sed 's#\(.*\)[/][^/]*$#\1#'
#echo "f2."
#git diff --name-only $1 $2 -- . 2> /dev/null | sed 's#\(.*\)[/][^/]*$#\1#' | sort -u
#echo "f3."
#git diff --name-only $1 $2 -- . 2> /dev/null | sed 's#\(.*\)[/][^/]*$#\1#' | sort -u | grep -v _scripts
#echo "ENd Files."

directories=`git diff --name-only $1 $2 -- . 2> /dev/null | sed 's#\(.*\)[/][^/]*$#\1#' | sort -u | grep -v _scripts | grep -v .github`
echo "Yo. Directories is $directories"
prefix=`pwd`
for g in $directories
do
    pushd $g
    pwd
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
    popd > /dev/null
    grammars+=( $g )
done

for grammar in ${grammars[@]}
do
    pushd $grammar
    mvn clean test --file pom.xml
    popd
done
