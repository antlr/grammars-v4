err=0
for g in `find ../<example_files_unix> -type f | grep -v '.errors$' | grep -v '.tree$'`
do
  file=$g
  x1="${g##*.}"
  if [ "$x1" != "errors" ]
  then
    echo $file
    node index.js -file $file
    status="$?"
    if [ -f "$file".errors ]
    then
      if [ "$stat" = "0" ]
      then
        echo Expected parse fail.
        err=1
      else
        echo Expected.
      fi
    else
      if [ "$status" != "0" ]
      then
        err=1
      fi
    fi
  fi
done
exit $err
