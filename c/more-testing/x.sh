set -x
set -e
cd ..
for t in Antlr4ng CSharp Dart Java TypeScript
do
	rm -rf Generated-*
	dotnet trgen -t $t
	cd Generated-$t
	make
	bash run.sh --DDBG -tree ../more-testing/x.c
	bash run.sh -tree ../more-testing/x.c
	cd ..
	rm -rf Generated-*
done	
