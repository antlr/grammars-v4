
# set -x
# set -e
dirs=`dotnet trxml2 ../desc.xml | grep targets | sed 's/.*[=]//' | tr ';' ' '`
p=`pwd`/x.c.p
cd ..
for t in $dirs
do
	echo $t
	rm -rf Generated-*
	dotnet trgen -t $t > /dev/null 2>&1
	cd Generated-$t
	make > /dev/null 2>&1
	rm -f $p
	o=`bash run.sh --DDBG -tree ../more-testing/x.c 2>&1 | grep 1`
	if [ "$o" == "" ]
	then
		echo Failed.
		exit 1
	fi
	rm $p
	o=`bash run.sh -tree ../more-testing/x.c  2>&1 | grep 0`
	if [ "$o" == "" ]
	then
		echo Failed.
		exit 1
	fi
	rm $p
	echo " => Looks ok."
	cd ..
	rm -rf Generated-*
done	
