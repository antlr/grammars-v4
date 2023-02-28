$JAR = '<antlr_tool_path>'
$CLASSPATH = "$JAR<if(path_sep_semi)>\;<else>:<endif>$pwd"
java -cp "$CLASSPATH" Test $args
