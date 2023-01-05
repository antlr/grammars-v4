# Template generated code from trgen <version>
err=0
SAVEIFS=$IFS
IFS=$(echo -en "\n\b")
files=`find ../<example_files_unix> -type f | grep -v '.errors$' | grep -v '.tree$'`
echo "$files" | trwdog ./bin/Debug/net6.0/<exec_name> -x -shunt -tree
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
git diff --exit-code --name-only . > $old/temp-output.txt 2>&1
diffs=$?
if [ "$diffs" = "129" ]
then
  err=$status
elif [ "$diffs" = "1" ]
then
  cat $old/temp-output.txt
  echo Output difference--failed test.
  err=1
else
  err=$status
fi
rm -f $old/temp-output.txt
exit $err
