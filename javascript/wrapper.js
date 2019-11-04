// eslump fuzz test warpper. obviousely we haven't pass it.
var cp = require("child_process");
var process = require("process");
var fs = require("fs");
module.exports = ({
    code,
    sourceType,
    reproductionData = {}
}) => {
    fs.writeFileSync("gen/temp.js", code);
    var child = cp.execSync("cd gen && grun JavaScript program temp.js 2>&1 1>NUL").toString()
    
    if (child.length > 0) {
        console.log (sourceType,child)
        return {
            child,
            reproductionData
        };
    }
};
