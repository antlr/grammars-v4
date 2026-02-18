
# Gather data.
for t in Antlr4ng Cpp CSharp Dart Go Java JavaScript Python3 TypeScript
do
    rm -rf Generated-$t*
    dotnet trgen -t $t
    pushd Generated-$t-1
    make
    bash perf.sh 20 group
    popd
done
