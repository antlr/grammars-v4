<if(antlr_is_dev)>
$ANTLR4_DEV_DIR = "<antlr_dev_dir>/antlr4"
$JAR = (Get-ChildItem "$ANTLR4_DEV_DIR/tool/target/antlr4-*-complete.jar" | Select-Object -Last 1).FullName
<else>
$version = "<antlr_version>"
$JAR = python -c "import os; from pathlib import Path; print(os.path.join(Path.home(), '.m2', 'repository', 'org', 'antlr', 'antlr4', '$version', ('antlr4-' + '$version' + '-complete.jar')))"
<endif>
$CLASSPATH = "$JAR<if(path_sep_semi)>\;<else>:<endif>$pwd"
java -cp "$CLASSPATH" Test $args
