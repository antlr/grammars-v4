import fs from 'fs-extra';
import exec from 'child_process';

// Read package.json
const packageJson = JSON.parse(fs.readFileSync('./package.json', 'utf8'));

// Extract the antlr4 version
const antlrVersion = packageJson.dependencies.antlr4.replace('^', '');

// Run the command with the antlr4 version
<tool_grammar_tuples:{x |
exec.execSync(`antlr4 -v ${antlrVersion\} -encoding <antlr_encoding> -Dlanguage=JavaScript <x.AntlrArgs> <antlr_tool_args:{y | <y> } > <x.GrammarFileName>`, (err, stdout, stderr) => {
    console.log("stdout: " + stdout);
    console.log("stderr: " + stderr);
\});
} >
