command -v mvn >/dev/null 2>&1 || { echo >&2 "I require maven to test the grammars but it's not installed.  Aborting."; exit 1; }
mvn test $@
