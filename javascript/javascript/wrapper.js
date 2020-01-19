// eslump fuzz test warpper. we haven't pass it yet...
var cp = require("child_process");
var fs = require("fs");
var os = require("os");
module.exports = ({
    code,
    sourceType,
    reproductionData = {}
}) => {
    fs.writeFileSync("gen/temp.js", code);
    var posixcmd = "cd gen && grun JavaScript program temp.js 2>&1 1>/dev/null";
    var cmd = {
        aix: posixcmd,
        // android: posixcmd
        darwin: posixcmd,
        freebsd: posixcmd,
        linux: posixcmd,
        openbsd: posixcmd,
        // sunos: posixcmd,
        win32: "cd gen && grun JavaScript program temp.js 2>&1 1>NUL",

    }
    var child = cp.execSync(cmd[os.platform()]).toString()
    
    if (child.length > 0) {
        console.log('')
        console.log(child)
        return {
            child,
            reproductionData
        };
    }
};
