# Template generated code from trgen <version>
err=0
SAVEIFS=$IFS
IFS=$(echo -en "\n\b")
stderr_file=`mktemp --tmpdir=. stderr.XXXXXXXXXX`
for g in `find ../<example_files_unix> -type f | grep -v '.errors$' | grep -v '.tree$'`
do
  file="$g"
  x1="${g##*.}"
  if [ "$x1" != "errors" ]
  then
    echo "$file"
    trwdog node index.js -file "$file" 2> "$stderr_file"
    status="$?"
    head -55 "$stderr_file"
    if [ -f "$file".errors ]
    then
      if [ "$status" = "0" ]
      then
        echo Expected parse fail.
        err=1
        break
      else
        echo Expected.
      fi
    else
      if [ "$status" != "0" ]
      then
        err=1
        break
      fi
    fi
  fi
done
rm "$stderr_file"
exit $err
