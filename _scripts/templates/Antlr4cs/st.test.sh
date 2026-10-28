# Generated from trgen <version>

# Uncomment for debugging.
#set -x

# glob patterns
shopt -s globstar

SAVEIFS=$IFS
IFS=$(echo -en "\n\b")

# Get environment.
unameOut="$(uname -s)"
case "${unameOut}" in
    Linux*)     machine=Linux;;
    Darwin*)    machine=Mac;;
    CYGWIN*)    machine=Cygwin;;
    MINGW*)     machine=MinGw;;
    *)          machine="UNKNOWN:${unameOut}"
esac

# Get a list of test files from the test directory. Do not include any
# .errors or .tree files. Pay close attention to remove only file names
# that end with the suffix .errors or .tree.
source_root='<source_root_unix>'
if command -v cygpath >/dev/null 2>&1; then source_root=$(cygpath -u "$source_root"); fi
source_examples="$source_root/<example_dir_unix>"
generated_dir=$(pwd -P)
output_dir="$generated_dir/examples"
trash() { (cd "$source_root" && dotnet trash "$@"); }
files2=`trash glob <source_glob_args_unix> | tr -d '\r' | grep -v '[.]errors$' | grep -v '[.]tree$' | grep -v '[.]trq$'`
files=()
for f in $files2
do
    if [ -d "$source_root/$f" ]; then continue; fi
        files+=( "$source_root/$f" )
done
parse_inputs=()
for f in "${files[@]}"; do
    if command -v cygpath >/dev/null 2>&1; then
        parse_inputs+=( "$(cygpath -m "$f")" )
    else
        parse_inputs+=( "$f" )
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

# Outputs belong to this generated app, never to the source examples.
mkdir -p "$output_dir"

# Parse all input files.
runner=("$generated_dir/bin/Debug/net10.0/<if(os_win)>Test.exe<else>Test<endif>")
<if(individual_parsing)>
# Individual parsing.
rm -f parse.txt
for f in "${parse_inputs[@]}"
do
    trash wdog "${runner[@]}" -q -tee -tree -o "$output_dir" "$f" >> parse.txt 2>&1
    xxx="$?"
    if [ "$xxx" -ne 0 ]
    then
        status="$xxx"
    fi
done
<else>
# Group parsing.
echo "${parse_inputs[*]}" | trash wdog "${runner[@]}" -q -x -tee -tree -o "$output_dir" > parse.txt 2>&1
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

# For Unix environments, convert the newline in the .errors and .trees
# to Unix style.
if [[ "$machine" == "MinGw" || "$machine" == "Msys" || "$machine" == "Cygwin" || "#machine" == "Linux" ]]
then
    find "$output_dir" -type f \( -name '*.errors' -o -name '*.tree' \) -exec dos2unix -q -f {} +
fi

# XQuery .trq assertions use trparse's generated CSharp parser and are
# checked by the CSharp test script. This target validates its own outputs.

# Compare generated results with the source examples without modifying them.
difference=0
for file in "${files[@]}"
do
    relative=${file#"$source_examples"/}
    if [ "$relative" = "$file" ]; then
        echo "Input is outside the example directory: $file"
        difference=1
        continue
    fi
    for extension in errors tree
    do
        expected="$file.$extension"
        actual="$output_dir/$relative.$extension"
        if [ -f "$expected" ]; then
            if [ ! -f "$actual" ]; then
                echo "Missing output: $actual (expected $expected)"
                difference=1
            elif ! diff -u --strip-trailing-cr -- "$expected" "$actual"; then
                difference=1
            fi
        elif [ "$extension" = errors ] && [ -s "$actual" ]; then
            echo "Unexpected parse errors for $file:"
            cat -- "$actual"
            difference=1
        fi
    done
done

if [ "$difference" -ne 0 ]; then
    echo "Test failed."
    exit 1
fi
echo "Test succeeded."
exit 0
