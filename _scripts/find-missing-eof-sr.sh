#!/bin/sh

test=""
# files=`find . -name pom.xml | grep -v Generated | grep -v -E -e "save/|agp/|asm/asm8086/|asn/asn/|bcpl/|clif/|cql3/|databank/|html/|inf/|lark/|metamath/|metric/|mumps/|pdn/|powerquery/|rfc1960/|scala/|scss/|sickbay/|sql/hive/v2/|sql/hive/v3/|stringtemplate/|tcpheader/|turing/|verilog/verilog/|verilog/systemverilog/|wavefront/|wkt/|rfc1035/"`
files=`find . -name pom.xml | grep -v Generated | grep -v -E -e "save/|.ignore/|Generated/"`
refined_list=`grep -l -i -e entrypoint $files`
for i in $refined_list
do
	echo $i
	dir=${i%/*}
	grammars=`trxml2 $i | grep -i 'includes/include=' | sed 's%.*/include=\(.*\)$%\1%'`
	sourcedir=`trxml2 $i | grep -i 'configuration/sourceDirectory=' | sed 's%.*/sourceDirectory=\(.*\)$%\1%' | sed 's/[$][{]basedir[}]//' | sed 's%^/%%g'`
	fixedgrammars=""
	for g in $grammars
	do
		if [[ $sourcedir == "" ]]
		then
			fixedgrammars="$fixedgrammars $g"
		else
			fixedgrammars="$fixedgrammars $sourcedir/$g"
		fi
	done
	entry=`trxml2 $i | grep -i 'configuration/entryPoint=' | sed 's%.*/entryPoint=\(.*\)$%\1%'`
	pushd $dir > /dev/null 2>&1
	for e in $entry
	do
		rule=`trparse -t antlr4 $fixedgrammars | trxgrep ' //parserRuleSpec[RULE_REF/text()="'$e'" and not(*//TOKEN_REF/text()="EOF")]' | trtext`
		if [[ $rule != "" ]]
		then
			echo Missing EOF in start rule $entry for grammar $dir
			echo $rule
			echo ""
			if [[ $test != "" ]]
			then
				rm -rf Generated
				trgen
				cd Generated
				echo $e
				trparse -t antlr4 *.g4 | trinsert " //parserRuleSpec[RULE_REF/text()='"$e"']/SEMI" "EOF" | trsponge -c
				make
				make test
				cd ..
				rm -rf Generated
			fi
		fi
	done
	
	popd > /dev/null 2>&1
done
