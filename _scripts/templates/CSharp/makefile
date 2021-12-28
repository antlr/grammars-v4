# Generated code from trgen <version>
GENERATED = <tool_grammar_tuples:{x|<x.GeneratedFileName> }>
default:
	dotnet restore
	dotnet build
run:
	trwdog dotnet run $(RUNARGS)
clean:
	dotnet clean
	rm -rf bin obj
test:
	bash test.sh
