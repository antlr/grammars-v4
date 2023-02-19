# Generated from trgen <version>
if [ -f transformGrammar.py ]; then python3 transformGrammar.py ; fi
dotnet restore
dotnet build
