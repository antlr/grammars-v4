# Template generated code from trgen <version>
err=0
SAVEIFS=$IFS
IFS=$(echo -en "\n\b")
stderr_file=`mktemp --tmpdir=. stderr.XXXXXXXXXX`
parse_out_file=`mktemp --tmpdir=. parse_out.XXXXXXXXXX`
for g in `find ../<example_files_unix> -type f | grep -v '.errors$' | grep -v '.tree$'`
do
  file="$g"
  x1="${g##*.}"
  if [ "$x1" != "errors" ]
  then
    echo "$file"
    trwdog node index.js -file "$file" -tree > "$parse_out_file" 2> "$stderr_file"
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
        diff \<(tr -d "\r\n" \< "$file".errors) \<(tr -d "\r\n" \< "$parse_out_file")
	    status="$?"
        if [ "$status" = "0" ]
        then
          echo Parse errors match succeeded.
        else
          echo Expected parse errors match.
          err=1
          break
        fi
      fi
    else # No .errors file
      if [ "$status" != "0" ]
      then
        err=1
        break
      fi
      if [ -f "$file".tree ]
      then
        diff \<(tr -d "\r\n" \< "$file".tree) \<(tr -d "\r\n" \< "$parse_out_file")
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
    if [ -f "$file".tree ]
    then
      diff \<(tr -d "\r\n" \< "$file".tree) \<(tr -d "\r\n" \< "$tree_file")
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
rm "$stderr_file" "$parse_out_file"
exit $err
