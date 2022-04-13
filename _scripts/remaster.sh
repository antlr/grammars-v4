#

list=`find examples -name '*.tree'`
trgen -t Java
cd Generated; make

for i in $list
do
    echo ${i%.tree}
    java -classpath /tmp/antlr4-4.10-complete.jar:. Program -tree -file ../${i%.tree} > ../$i
    truncate -s-1 ../$i
done



