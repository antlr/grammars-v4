# Template generated code from trgen <version>
err=0
SAVEIFS=$IFS
IFS=$(echo -en "\n\b")
tree_file=`mktemp --tmpdir=. tree.XXXXXXXXXX`
for g in `find ../<example_files_unix> -type f | grep -v '.errors$' | grep -v '.tree$'`
do
  file="$g"
  x1="${g##*.}"
  if [ "$x1" != "errors" ]
  then
    echo "$file"
    trwdog <if(os_win)>./cli.exe<else>./cli<endif> -file "$file" -tree > "$tree_file"
    status="$?"
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
    if [ -f "$file".tree ]
    then
      diff -q "$file".tree "$tree_file" > /dev/null
	  status="$?"
      if [ "$status" = "0" ]
      then
        echo Parse tree match succeeded.
      else
        echo Expected parse tree match.
        err=1
        break
      fi
    fi
  fi
done
rm "$tree_file"
exit $err
