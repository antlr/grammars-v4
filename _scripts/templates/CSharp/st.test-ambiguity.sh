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
<if(individual_parsing)>
# Individual parsing: NOT SUPPORTED!
<else>
# Group parsing.
echo "${files[*]}" | dotnet trwdog -- dotnet trperf -- -x -c ar | grep -v '^0' | awk '{print $2}' | sort -u
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
