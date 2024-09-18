# Generated from trgen <version>

SAVEIFS=$IFS
IFS=$(echo -en "\n\b")

# Get a list of test files from the test directory. Do not include any
# .errors or .tree files. Pay close attention to remove only file names
# that end with the suffix .errors or .tree.
files2=`dotnet trglob -- '../<example_files_unix>' | grep -v '.errors$' | grep -v '.tree$'`
files=()
for f in $files2
do
    if [ -d "$f" ]; then continue; fi
    dotnet triconv -- -f utf-8 $f > /dev/null 2>&1
    if [ "$?" = "0" ]
    then
        files+=( $f )
    fi
done

# People often specify a test file directory, but sometimes no
# tests are provided. Git won't check in an empty directory.
# Test if there are no test files.
if [ ${#files[@]} -eq 0 ]
then
    echo "No test cases provided."
    exit 0
fi

# Parse all input files.
# Individual parsing.
rm -f parse.txt
for f in ${files[*]}
do
    trwdog python3 Test.py -prefix individual $f >> parse.txt 2>&1
    xxx="$?"
    if [ "$xxx" -ne 0 ]
    then
        status="$xxx"
    fi
done
# Group parsing.
echo "${files[*]}" | trwdog python3 Test.py -x -prefix group >> parse.txt 2>&1
status=$?

exit 0
