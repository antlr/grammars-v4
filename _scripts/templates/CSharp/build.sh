# Generated from trgen <version>
set -e
if [ -f transformGrammar.py ]; then python3 transformGrammar.py ; fi
dotnet restore Test.csproj
dotnet build Test.csproj
exit 0
