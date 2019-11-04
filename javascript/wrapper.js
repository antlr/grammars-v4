// eslump fuzz test warpper. we haven't pass it yet...
var cp = require("child_process");
var fs = require("fs");
module.exports = ({
    code,
    sourceType,
    reproductionData = {}
}) => {
    fs.writeFileSync("gen/temp.js", code);
    var child = cp.execSync("cd gen && grun JavaScript program temp.js 2>&1 1>NUL").toString()
    
    if (child.length > 0) {
        console.log('')
        console.log(child)
        return {
            child,
            reproductionData
        };
    }
};
