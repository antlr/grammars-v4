# Generated from trgen <version>

SAVEIFS=$IFS
IFS=$(echo -en "\n\b")

rm -f parse.txt

echo "Testing for TypeScript" >> parse.txt
echo "<parser_name>" >> parse.txt
date >> parse.txt
echo "" >> parse.txt

# Output basic information on machine.
unameOut="$(uname -s)"
case "${unameOut}" in
    Linux*)     machine=Linux;;
    Darwin*)    machine=Mac;;
    CYGWIN*)    machine=Cygwin;;
    MINGW*)     machine=MinGw;;
    *)          machine="UNKNOWN:${unameOut}"
esac

echo OS: >> parse.txt
if [[ "$machine" == "Linux" ]]
then
    lsb_release -a >> parse.txt
fi
if [[ "$machine" == "CYGWIN" || "$machine" == "MinGw" ]]
then
    systeminfo | grep -E '^OS' >> parse.txt
fi
echo "" >> parse.txt

echo CPU: >> parse.txt
if [[ "$machine" == "Linux" ]]
then
    lscpu | grep -e 'Model name' >> parse.txt
fi
if [[ "$machine" == "CYGWIN" || "$machine" == "MinGw" ]]
then
    pwsh -c '(Get-WmiObject -Class Win32_Processor).Name' >> parse.txt
fi
echo "" >> parse.txt

echo Memory: >> parse.txt
if [[ "$machine" == "Linux" ]]
then
    free -h >> parse.txt
fi
if [[ "$machine" == "CYGWIN" || "$machine" == "MinGw" ]]
then
    pwsh -c '(Get-WmiObject -Class Win32_PhysicalMemory).Capacity' >> parse.txt
fi
echo "" >> parse.txt

# Get a list of test files from the test directory. Do not include any
# .errors or .tree files. Pay close attention to remove only file names
# that end with the suffix .errors or .tree.
files2=`dotnet trglob '../<example_files_unix>' | grep -v '.errors$' | grep -v '.tree$'`
files=()
for f in $files2
do
    if [ -d "$f" ]; then continue; fi
    dotnet triconv -f utf-8 $f > /dev/null 2>&1
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

n=$1
type=$2
echo SampleSize=$n >> parse.txt
echo "" >> parse.txt

# Parse all input files.
# Individual parsing.
if [[ "$type" == "individual" ]]
then
    for f in ${files[*]}
    do
        # Loop from 1 to n and execute the body of the loop each time
        for ((i=1; i\<=n; i++))
        do
            dotnet trwdog sh -c "exec npx tsx Test.js -prefix individual $f" >> parse.txt 2>&1
            xxx="$?"
            if [ "$xxx" -ne 0 ]
            then
                status="$xxx"
            fi
        done
    done
fi

# Group parsing.
# Loop from 1 to n and execute the body of the loop each time
if [[ "$type" == "group" ]]
then
    for ((i=1; i\<=n; i++))
    do
        echo "${files[*]}" | dotnet trwdog sh -c "exec npx tsx Test.js -x -prefix group" >> parse.txt 2>&1
        xxx="$?"
        if [ "$xxx" -ne 0 ]
        then
            status="$xxx"
        fi
    done
fi

dos2unix parse.txt

exit 0
