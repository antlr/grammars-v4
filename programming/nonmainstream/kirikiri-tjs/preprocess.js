// Node.js v10

// Minimal preprocessor for TJS2
// See https://krkrz.github.io/krkr2doc/tjs2doc/contents/pp.html
const fs = require('fs');
const vm = require('vm');

const fstr = fs.readFileSync(0).toString(); // STDIN=0
const lines = fstr.split(/\r?\n/);

// You may need modify this
let sandbox = {
    version: 0x02040009,
    kirikiriz: 1,
    environment: 0,

    KAGHOOK_EVENTACTION: 0,
    APPEND_EXTENTION: 0,
    DEBUG: 0,
    PACKED: 0,
    SYSVOICE_USE_CLEARTYPEVOICE: 0,
};
vm.createContext(sandbox);
const exec = (code) => {
    try {
        return vm.runInContext(code, sandbox);
    } catch(e){
        console.error(`// Preprocessor: ${e.toString()}`)
    }
}

let canOutput = true;
const mapped = lines.map((l, i) => {
    const line = l.trim()
    let writeback = l;
    if (!canOutput) writeback = '//' + l;
    if (line[0] === '@' && (line[1] !== '"' && line[1] !== "'")) {
        const match = line.match(/\@([^(]*)(\(.*\))?/);
        writeback = '// ' + l;
        switch (match[1].trim()) {
            case "if":
                if (!canOutput) break;
                const cond = match[2];
                const result = exec(cond);
                if (!result) {
                    canOutput = false;
                }
                break;
            case "endif":
                canOutput = true;
                break;
            case "set":
                if (!canOutput) break;
                exec(match[2]);
                break;
            default:
                // console.error(` Preprocessor unknown command at ${i}: '${line}', '${match[1]}'`);
                writeback = l;
                break;
        }
    }
    return writeback;
});

console.log(mapped.join('\n'));