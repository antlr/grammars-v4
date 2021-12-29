#! /bin/bash

. clean.sh
. build.sh
ERR=lasterror.txt

DEFAULTTEST=example/**/*.tjs
TEST=${@:-$DEFAULTTEST}
FAIL=0
time for i in $TEST
do
    echo Test $i
    cat $i | node preprocess.js | grun TJS program 2> $ERR
    if [[ `cat $ERR |wc -l` != 0 ]]
    then
        echo $i failed
        head -n 2 -c 160 $ERR
        FAIL=1
        echo 
        break
    fi
done
if [[ $FAIL == 0 ]]
then
    echo All test passed
else
    echo One test failed
fi
rm $ERR
. clean.sh
