#!/bin/sh

echo "YO. I AM HERE."

grammars=()
# Test grammars for the enclosing directories.
directories=`git diff --name-only $1 $2 . 2> /dev/null | sed 's#\(.*\)[/][^/]*$#\1#' | sort -u | grep -v _scripts`
for g in $directories
do
    pushd $g > /dev/null
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
    g=${g##*$prefix}
    g=${g##/}
    if [ "$g" == "" ]
    then
	g="."
    fi
    popd > /dev/null
    echo Adding diff $g
    grammars+=( $g )
done
echo "YO. Here 2."

for grammar in ${grammars[@]}
do
    pushd $grammar
    mvn -B package --file pom.xml
    popd
done

echo "YO. Done."
