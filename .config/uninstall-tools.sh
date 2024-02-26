#
tools=$(cat .config/dotnet-tools.json | jq -r '.tools[] | .commands[0]')

for tool in $tools
do
    t=`echo $tool | awk '{print $1}'`
    echo "t = '"$t"'"
    dotnet tool uninstall $t
done
cp .config/dotnet-tools.json.save .config/dotnet-tools.json
dotnet nuget locals all --clear
