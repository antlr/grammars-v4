
# This must be executed at the root level of the repo.
# We assume that you are there.

# Also assumes you nuked the Generated/ directories.
# Not sure how you'd do that in Pwsh as there's no
# easy "rm -rf `find . -name Generated -type de`"
# in Pwsh.

# Download Antlr .jar.
# Parameter to get-antlr.ps1 must be in double quotes.
$antlrPath = _scripts/get-antlr.ps1 "4.11.1"

# Set up env as it is used in test script.
echo "antlr_path=$antlrPath" >> $env:GITHUB_ENV

dotnet tool install -g trgen --version 0.19.0
dotnet tool install -g triconv --version 0.19.0
dotnet tool install -g trwdog --version 0.19.0

# Call test script.
$env:ANTLR_JAR_PATH="$antlrPath"
_scripts/test.ps1 $args[0]

