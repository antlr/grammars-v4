# Generated from trgen <version>
set -e
set -x

exec npx tsx Test.js "$@"

exit $?
