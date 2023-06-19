# Generated from trgen <version>

# People often specify a test file directory, but sometimes no
# tests are provided. Git won't check in an empty directory.
# Test if the test file directory does not exist, or it is just
# an empty directory.
if [ ! -d ../<example_files_unix> ]
then
    echo "No test cases provided."
    exit 0
elif [ ! "$(ls -A ../<example_files_unix>)" ]
then
    echo "No test cases provided."
    exit 0
fi

SAVEIFS=$IFS
IFS=$(echo -en "\n\b")

# Get a list of test files from the test directory. Do not include any
# .errors or .tree files. Pay close attention to remove only file names
# that end with the suffix .errors or .tree.
files2=`find ../<example_files_unix> -type f | grep -v '.errors$' | grep -v '.tree$'`
files=()
for f in $files2
do
    triconv -f utf-8 $f > /dev/null 2>&1
    if [ "$?" = "0" ]
    then
        files+=( $f )
    fi
done

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
