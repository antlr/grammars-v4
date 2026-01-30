
# Gather data.
for t in Antlr4ng CSharp Dart Java TypeScript
do
    rm -rf Generated-$t*
    dotnet trgen -t $t
    pushd Generated-$t
    make
    bash perf.sh 20 group
    popd
done
