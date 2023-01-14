# Template generated code from trgen <version>

if [ ! -d ../<example_files_unix> ]
then
    echo "No test cases provided."
    exit 0
else
    if [ ! "$(ls -A ../<example_files_unix>)" ]
    then
        echo "No test cases provided."
        exit 0
    fi
fi

SAVEIFS=$IFS
IFS=$(echo -en "\n\b")

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

# Parse
echo "${files[*]}" | trwdog node Test.js -x -tee -tree
status=$?

# trwdog returns 255 if it cannot spawn the process.
if [ "$status" = "255" ]
then
  echo "Test failed."
  exit 1
fi

# rm -rf `find ../<example_files_unix> -type f -name '*.errors' -o -name '*.tree' -size 0`
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

# Check if any .errors/.tree files have changed. That's not good.
git diff --exit-code --name-only . > $old/updated.txt 2>&1
updated=$?

# Check if any untracked .errors files are not empty. That's also not good.
git ls-files --exclude-standard -o > $old/new_errors2.txt 2>&1
new_errors=$?
for f in `cat $old/new_errors2.txt`
do
  ext=${f##*.}
  ext=".$ext"
  if [ "$ext" = ".errors" ]
  then  
    if [ -s $f ]
    then
      echo $f >> $old/new_errors.txt
    fi
  fi
done

if [ "$updated" = "129" ]
then
  echo "Grammar outside a git repository. Assuming parse exit code."
  if [ "$status" = 0 ]
  then
    echo "Test succeeded."
  else
    echo "Test failed."
  fi
  rm -f $old/updated.txt $old/new_errors2.txt $old/new_errors.txt
  exit $status
fi

if [ "$updated" = "1" ]
then
  echo "Difference in output."
  echo "Test failed."
  rm -f $old/updated.txt $old/new_errors2.txt $old/new_errors.txt
  exit 1
fi

if [ -s $old/new_errors.txt ]
then
  echo "New errors in output."
  echo "Test failed."
  rm -f $old/updated.txt $old/new_errors2.txt $old/new_errors.txt
  exit 1
fi

echo "Test succeeded."
rm -f $old/updated.txt $old/new_errors2.txt $old/new_errors.txt
exit 0
