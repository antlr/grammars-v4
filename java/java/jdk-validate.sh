#!/bin/sh
set -x
set -e

cwd=`pwd`
root=${cwd}
echo $root
pushd $root
if [ ! -f desc.xml ]
then
	echo Unexpected root directory for the java grammar.
	exit 1
fi
popd

url=https://github.com/openjdk/jdk/archive/refs/tags/jdk-21-ga.zip
filename=$(basename "$url")
name="jdk-${filename%.*}"

if [[ ! -f jdk-21-ga.zip ]]
then
	wget $url
fi

if [[ ! -d jdk-jdk-21-ga ]]
then
	unzip "$filename" > /dev/null 2>&1
fi

dotnet trash gen -t CSharp
cd Generated-*
make
for times in 1 2 3
do
	time ( find $cwd/$name/src -name '*.java' | xargs cygpath -w | bash run.sh -x 2>&1 | egrep "^TT:" )
done
