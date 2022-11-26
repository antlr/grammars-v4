#

if [ ! -d cpython ]
then
    git clone https://github.com/python/cpython.git
fi
files=`find cpython -name '*.py'`
total=0
failed=0
for i in $files
do
    total=$((total+1))
    ls -l $i
    trparse -e $i
    status=$?
    if [ $status != "0" ]
    then
        failed=$((failed+1))
        echo $((100*failed/total))
    fi
done
echo $((failed*100/total))
