# Generated from trgen <version>
set -e
if [ -f transformGrammar.py ]; then python3 transformGrammar.py ; fi
rm -rf build
mkdir build
cd build
cmake .. <cmake_target>
<if(os_win)>cmake --build . --config Release<else>$(MAKE)<endif>
exit 0
