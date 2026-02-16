#
set -x
set -e
files=`find ../examples/acats -name '*.ada'`
for f in $files
do
	echo $f
	grep -i procedure $f
	./bin/Debug/net10.0/Test.exe $f
done
