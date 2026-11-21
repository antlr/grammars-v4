import {test} from 'test';

namespace Name {
    enum Values {
        val1 = 3,
        val2
    }
}

interface Inter {
    value: number;
}

class Class {
    private x;

    constructor(x) {
        this.x = x;
    }

    public test(y: number) {
        return this.x + y;
    }
}

let a = 3;
const d = 4;

function c() {
    let b;
    b = 8;
    return a - b;
}

switch (c()) {
    case 1:
        break;
    default:
        console.log(c())
}

try {
    throw "Error"
} catch (e) {
    console.log('Error', e);
} finally {
    console.log()
}

for (let i = 0; i < a; i++) {

}

while(a > 3) {
    a--;
    if (d > 5) {
        continue;
    }
}

export default Name;
export { c };

if (d) {

} else if (d < 10) {

} else {
    
}