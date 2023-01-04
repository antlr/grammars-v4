# Template generated code from trgen <version>
err=0
SAVEIFS=$IFS
IFS=$(echo -en "\n\b")
parse_out_file=`mktemp --tmpdir=. parse_out.XXXXXXXXXX`
files=`find ../<example_files_unix> -type f | grep -v '.errors$' | grep -v '.tree$'`

echo "$files" | trwdog ./bin/Debug/net6.0/<exec_name> -x -tree > "$parse_out_file"
status="$?"
if [ -f "$file".errors ]
then
  if [ "$status" = "0" ]
  then
    echo Expected parse fail.
    err=1
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
    fi
  fi
else # No .errors file
  if [ "$status" != "0" ]
  then
    err=1
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
    fi
  fi
fi
#rm "$parse_out_file"
exit $err
