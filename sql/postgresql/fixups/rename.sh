names=`dotnet trash parse PostgreSQLParser.g4 | dotnet trash nullable | \
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
dotnet trash parse PostgreSQLParser.g4 | dotnet trash rename "$rename" | dotnet trash sponge -o . -c
