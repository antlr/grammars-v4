
# Gather data.
for t in Antlr4ng CSharp Cpp Dart Go Java JavaScript TypeScript Python3
do
    dotnet trgen -t $t
    pushd Generated-$t-0
    make
    bash perf.sh 5 group
    popd
done
