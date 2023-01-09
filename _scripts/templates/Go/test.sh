# Template generated code from trgen <version>
err=0
SAVEIFS=$IFS
IFS=$(echo -en "\n\b")
files=`find ../<example_files_unix> -type f | grep -v '.errors$' | grep -v '.tree$'`
before_parse_errors=`find ../examples -type f -name '*.errors' -size +0`

# Parse
echo "$files" | trwdog ./<exec_name> -x -shunt -tree
status=$?

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
  gen=`find ../examples -type f -name '*.errors' -o -name '*.tree'`
  if [ "$gen" != "" ]
  then
    dos2unix $gen
  fi
fi
old=`pwd`
cd ../<example_files_unix>
# Notes: If the .tree and .errors files are not checked in,
# then git won't complain. The trick here is to make sure we
# don't have any new .errors files. Check the count again and
# make sure that hasn't changed.
git diff --exit-code --name-only . > $old/temp-output.txt 2>&1
diffs=$?
after_parse_errors=`find ../examples -type f -name '*.errors' -size +0`
if [ "$diffs" = "129" ]
then
  echo "Grammar outside a git repository."
  echo "Defaulting to exit code of group parse."
  err=$status
elif [ "$diffs" = "1" ]
then
  cat $old/temp-output.txt
  echo "Difference in output."
  err=1
elif [ "$before_parse_errors" != "$after_parse_errors" ]
then
  for f in $after_parse_errors
  do
	cat $f
  done
  echo "Difference in output."
  echo "Test failed."
  err=1
else
  echo "Test succeeded."
  err=0
fi
rm -f $old/temp-output.txt
exit $err
