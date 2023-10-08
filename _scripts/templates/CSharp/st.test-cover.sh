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
    dotnet triconv -- -f utf-8 $f > /dev/null 2>&1
    if [ "$?" = "0" ]
    then
        files+=( $f )
    fi
done

# Parse all input files.
<if(individual_parsing)>
# Individual parsing: NOT SUPPORTED!
<else>
# Group parsing.
echo "${files[*]}" | dotnet trwdog -- dotnet trcover -- -x
status=$?
<endif>

# trwdog returns 255 if it cannot spawn the process. This could happen
# if the environment for running the program does not exist, or the
# program did not build.
if [ "$status" = "255" ]
then
    echo "Test failed."
    exit 1
fi

rm -f $old/updated.txt $old/new_errors2.txt $old/new_errors.txt
exit 0
