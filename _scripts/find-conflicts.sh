#!/bin/sh

names=`trparse Sparql.g4 | trxgrep ' //parserRuleSpec/RULE_REF/text()' | sort -u | dos2unix | grep -v -E -e '_$' | grep -v -E -e '^query$'`
newnames=""
for i in $names
do
	newnames="$newnames"$i,$i"_;"
done
newnames="${newnames::-1}"
trparse Sparql.g4 | trrename -r "$newnames" | trsponge -c
for i in $names
do
	fixed=$i'_'
	echo Testing $fixed '=>' $i
	rm -rf testit
	mkdir testit
	pushd testit > /dev/null 2>&1
	cp ../Sparql.g4 .
	trparse Sparql.g4 | trrename -r "$fixed,$i" | trsponge -c > /dev/null 2>&1
	cp -r ../examples .
	cp ../pom.xml .
	trgen -t Go > /dev/null 2>&1
	cd Generated
	make > /dev/null 2>&1
	if [[ "$?" != "0" ]]
	then
		echo "==========================="
		echo $i did not work.
	fi
	cd ..
	rm -rf Generated
	popd > /dev/null 2>&1
	rm -rf testit

done
