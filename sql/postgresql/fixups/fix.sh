#
# set -x
set -e
set -o pipefail
for dir in `find . -name desc.xml | sed 's#/desc.xml##' | sort -u`
do
	echo $dir
	# Find rules that contain top-level empty alts.
	# Note, not complete because the alt may be not empty, but could derive empty.
	dotnet trparse -l $dir/*.g4 2>/dev/null > save.pt
	cat save.pt | dotnet trxgrep ' //parserRuleSpec[./ruleBlock/ruleAltList/labeledAlt/alternative[count(./*) = 0]]/RULE_REF/text()' > rules.txt
	# Find locations of use that an operator applied to it.
	for r in `cat rules.txt`
	do
		rr=`echo $r | tr -d '\n' | tr -d '\r'`
		echo Working on $rr
		dotnet trparse $dir/*.g4 2>/dev/null | dotnet trquery replace ' //parserRuleSpec/ruleBlock//element[./atom and not(./ebnfSuffix)]/atom/ruleref/RULE_REF[./text() = "'$rr'"]' "'$rr?'" | dotnet trsponge -o $dir -c
		# Remove empty alt in the rule.
		dotnet trparse $dir/*.g4 2>/dev/null | dotnet trquery delete "  //parserRuleSpec[RULE_REF/text() = '$rr']/ruleBlock/ruleAltList/labeledAlt[alternative/count(./*) = 0 and ./preceding-sibling::*[last()]/self::OR]/(. | ./preceding-sibling::OR[last()])" | dotnet trsponge -o $dir -c
		dotnet trparse $dir/*.g4 2>/dev/null | dotnet trquery delete "  //parserRuleSpec[RULE_REF/text() = '$rr']/ruleBlock/ruleAltList/labeledAlt[alternative/count(./*) = 0 and ./following-sibling::*[1]/self::OR]/(. | ./following-sibling::OR[1])" | dotnet trsponge -o $dir -c
	done
done
