#
# set -x
set -e
set -o pipefail
for dir in `find . -name desc.xml | sed 's#/desc.xml##' | sort -u`
do
	# Find rules that contain top-level empty alts.
	# Note, not complete because the alt may be not empty, but could derive empty.
	dotnet trparse -l $dir/*.g4 2>/dev/null | dotnet trxgrep ' //parserRuleSpec[./ruleBlock/ruleAltList/labeledAlt/alternative[count(./*) = 0]]/RULE_REF' | dotnet trcaret
done
