#

list=`find examples -name '*.tree'`
dotnet trgen -- -t Java
cd Generated; make

for i in $list
do
    echo ${i%.tree}
    java -classpath /tmp/antlr-4.9.3-complete.jar:. Program -tree -file ../${i%.tree} > ../$i
    truncate -s-1 ../$i
done



