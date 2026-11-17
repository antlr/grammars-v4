# Generated from trgen <version>
set -e
if [ "${ANTLR_GENERATION_ONLY:-0}" = "1" ]; then set -x; fi
if [ -f transformGrammar.py ]; then python3 transformGrammar.py ; fi
rm -rf node_modules
rm -rf build
mkdir build
cd build
<if(antlrng_tool)>
npm init -y
npm i antlr-ng
<endif>
<if(test.IsWindows)>cmake .. -G "Visual Studio 18 2026" -A x64
<else>cmake ..
<endif>
if [ "${ANTLR_GENERATION_ONLY:-0}" = "1" ]; then
cmake --build . --target antlr_warning_inventory --verbose
exit 0
fi
<if(test.IsWindows)>cmake --build . --config Release
<else>make
<endif>
exit 0
