names=`dotnet trparse PostgreSQLParser.g4 | dotnet trnullable | \
	grep ' False' | grep '^opt_' | awk '{print $1}'`
for i in $names
do
#	echo got $i
	old=$i
	new=${old:4}_
#	echo old $old new $new
	rename="$old,$new${sep:=}$rename"
	sep=";"
done
echo $rename
dotnet trparse PostgreSQLParser.g4 | dotnet trrename "$rename" | dotnet trsponge -o . -c
