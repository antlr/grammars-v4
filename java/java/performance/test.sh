#!/bin/sh
set -x
set -e

zips=(
https://github.com/openjdk/jdk/archive/refs/tags/jdk8-b120.zip
)
#https://github.com/openjdk/jdk/archive/refs/tags/jdk-17-ga.zip

for url in ${zips[@]}
do
	wget $url
	filename=$(basename "$url")
	unzip "$filename" > /dev/null 2>&1
	pushd ..
	dotnet trgen -t CSharp
	cd Generated-*
	make
	for times in 1 2 3
	do
		time (find ../performance/$filename/src -name '*.java' | bash run.sh -x > /dev/null 2>&1)
	done
	cd ..
	rm -rf Generated-*
	popd
	rm -rf "$filename"
	name="${filename%.*}"
	rm -rf $name
done
