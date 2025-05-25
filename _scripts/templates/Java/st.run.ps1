$version = Select-String -Path "build.sh" -Pattern "version=" | ForEach-Object { $_.Line -split "=" | Select-Object -Last 1 }
$JAR = python -c "import os; from pathlib import Path; print(os.path.join(Path.home(), '.m2', 'repository', 'org', 'antlr', 'antlr4', '$version', ('antlr4-' + '$version' + '-complete.jar')))"
$CLASSPATH = "$JAR<if(path_sep_semi)>\;<else>:<endif>$pwd"
java -cp "$CLASSPATH" Test $args
