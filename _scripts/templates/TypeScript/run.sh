# Generated from trgen <version>
set -e
# set -x

# ts-node is a bash script, so duplicate that code and call node via trwdog.
tsnode=`which ts-node`

basedir=$(dirname "$(echo "$tsnode" | sed -e 's,\\\\,/,g')")

case `uname` in
    *CYGWIN*|*MINGW*|*MSYS*) basedir=`cygpath -w "$basedir"`;;
esac

if [ -x "$basedir/node" ]; then
  exec trwdog "$basedir/node" "$basedir/node_modules/ts-node/dist/bin.js" Test.js "$@"
else 
  exec trwdog node "$basedir/node_modules/ts-node/dist/bin.js" Test.js "$@"
fi
exit $?
