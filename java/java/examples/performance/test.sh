#!/bin/sh
set -x
set -e

cwd=`pwd`
root=${cwd%examples/*}
echo $root
pushd $root
if [ ! -f desc.xml ]
then
	echo Unexpected root directory for the java grammar.
	exit 1
fi
popd

zips=(
https://github.com/openjdk/jdk/archive/refs/tags/jdk8-b120.zip
)
#Current version of grammar cannot parse jdk 17.
#https://github.com/openjdk/jdk/archive/refs/tags/jdk-17-ga.zip

for url in ${zips[@]}
do
	wget $url
	filename=$(basename "$url")
	name="jdk-${filename%.*}"
	unzip "$filename" > /dev/null 2>&1
	pushd $root
	dotnet trgen -t CSharp
	cd Generated-*
	make
	for times in 1 2 3
	do
		time ( find $cwd/$name/jdk/src -name '*.java' | xargs cygpath -w | bash run.sh -x 2>&1 | grep "Total Time" )
	done
	cd ..
	rm -rf Generated-*
	popd
	rm -rf "$filename"
	rm -rf $name
done
