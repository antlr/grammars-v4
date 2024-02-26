#
#set -x
declare -A targets
for t in "Cpp" "CSharp" "Dart" "Go" "Java" "JavaScript" "PHP" "Python3" "TypeScript"
do
    targets[$t]=0
done
directories=( `git diff --name-only $1 $2 . 2> /dev/null | sed 's#\(.*\)[/][^/]*$#\1#' | sort -u | grep -v _scripts | fgrep -v .github | tr -d '\r'` )
prefix=`pwd`
for g in ${directories[@]}
do
    if [ ! -d "$g" ]
    then
        continue
    fi
    pushd $g > /dev/null 2> /dev/null
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
    if [ -f desc.xml ]
    then
        gtargets=`dotnet trxml2 -- desc.xml | fgrep -e '/desc/targets' | awk -F = '{print $2}' | sed 's/;/ /g'`
        for t in $gtargets
        do
            targets[$t]=`expr ${targets[$t]} + 1`
        done
    fi
    popd > /dev/null
done

ttargets=""
# remove temporarily TODO add back: "TypeScript"
for t in "Cpp" "CSharp" "Dart" "Go" "Java" "JavaScript" "PHP" "Python3"
do
    if [ ${targets[$t]} -ne 0 ]
    then
        if [ "$ttargets" == "" ]
        then
            ttargets="\"$t\""
        else
            ttargets="$ttargets, \"$t\""
        fi
    fi
done
if [ "$ttargets" == "" ]
then
    # Github Actions doesn't like a job matrix to contain an empty set.
    # Add Java as a default.
    ttargets="\"Java\""
fi
echo 'myoutput=[' $ttargets ']'
