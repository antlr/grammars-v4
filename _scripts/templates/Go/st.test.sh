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
# Individual parsing.
<if(individual_parsing)>
rm -f parse.txt
for f in ${files[*]}
do
    dotnet trwdog -- ./<if(os_win)>Test.exe<else>Test<endif> -q -tee -tree $f >> parse.txt 2>&1
    xxx="$?"
    if [ "$xxx" -ne 0 ]
    then
        status="$xxx"
    fi
done
<else>
# Group parsing.
echo "${files[*]}" | dotnet trwdog -- ./<if(os_win)>Test.exe<else>Test<endif> -q -x -tee -tree > parse.txt 2>&1
status=$?
<endif>

# trwdog returns 255 if it cannot spawn the process. This could happen
# if the environment for running the program does not exist, or the
# program did not build.
if [ "$status" = "255" ]
then
    echo "Test failed."
    cat parse.txt
    exit 1
fi

# Any parse errors will be put in .errors files. But, if there's any
# output from the program in stdout or stderr, it's all bad news.
if [ -s parse.txt ]
then
    echo "Test failed."
    cat parse.txt
    exit 1
fi

# rm -rf `find ../<example_files_unix> -type f -name '*.errors' -o -name '*.tree' -size 0`

# For Unix environments, convert the newline in the .errors and .trees
# to Unix style.
unameOut="$(uname -s)"
case "${unameOut}" in
    Linux*)     machine=Linux;;
    Darwin*)    machine=Mac;;
    CYGWIN*)    machine=Cygwin;;
    MINGW*)     machine=MinGw;;
    *)          machine="UNKNOWN:${unameOut}"
esac
if [[ "$machine" == "MinGw" || "$machine" == "Msys" || "$machine" == "Cygwin" || "#machine" == "Linux" ]]
then
    gen=`find ../<example_files_unix> -type f -name '*.errors' -o -name '*.tree'`
    if [ "$gen" != "" ]
    then
        dos2unix $gen
    fi
fi

old=`pwd`
cd ../<example_files_unix>

# Check if any files in the test files directory have changed.
git config --global pager.diff false
rm -f $old/updated.txt
updated=0
for f in `find . -name '*.errors'`
do
    git diff --exit-code $f >> $old/updated.txt 2>&1
    xxx=$?
    if [ "$xxx" -ne 0 ]
    then
        updated=$xxx
    fi
done
for f in `find . -name '*.tree'`
do
    git diff --exit-code $f >> $old/updated.txt 2>&1
    xxx=$?
    if [ "$xxx" -ne 0 ]
    then
        updated=$xxx
    fi
done

# Check if any untracked .errors files.
git ls-files --exclude-standard -o > $old/new_errors2.txt 2>&1
new_errors=$?

# Gather up all untracked .errors file output. These are new errors
# and must be reported as a parse fail.
rm -f $old/new_errors.txt
touch $old/new_errors.txt
for f in `cat $old/new_errors2.txt`
do
    ext=${f##*.}
    ext=".$ext"
    if [ "$ext" = ".errors" ]
    then  
        if [ -s $f ]
        then
            echo $f >> $old/new_errors.txt
            cat $f >> $old/new_errors.txt
        fi
    fi
done

# If "git diff" reported an exit code of 129, it is because
# the directory containing the grammar is not in a repo. In this
# case, assume parse error code as the defacto result.
if [ "$updated" = "129" ]
then
    echo "Grammar outside a git repository. Assuming parse exit code."
    if [ "$status" = 0 ]
    then
        echo "Test succeeded."
    else
        cat $old/new_errors.txt
        echo "Test failed."
    fi
    rm -f $old/updated.txt $old/new_errors2.txt $old/new_errors.txt
    exit $status
fi

# "Git diff" reported a difference. Redo the "git diff" to print out all
# the differences. Also, output any untracked, non-zero length .errors files.
if [ "$updated" = "1" ]
then
    echo "Difference in output."
    git diff .
    cat $old/new_errors.txt
    echo "Test failed."
    rm -f $old/updated.txt $old/new_errors2.txt $old/new_errors.txt
    exit 1
fi

# If there's non-zero length .errors flies that are new, report them
# as errors in the parse.
if [ -s $old/new_errors.txt ]
then
    echo "New errors in output."
    cat $old/new_errors.txt
    echo "Test failed."
    rm -f $old/updated.txt $old/new_errors2.txt $old/new_errors.txt
    exit 1
fi

echo "Test succeeded."
rm -f $old/updated.txt $old/new_errors2.txt $old/new_errors.txt
exit 0
