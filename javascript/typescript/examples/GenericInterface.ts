interface IProcessor
{
    result:T;
    process(a: T, b: T) => T;
}

interface KeyPair<T, U> {
    key: T;
    value: U;
}

let kv1: KeyPair<number, string> = { key:1, value:"Steve" }; // OK
let kv2: KeyPair<number, number> = { key:1, value:12345 }; // OK

interface KeyValueProcessor<T, U>
{
    (key: T, val: U): void;
};

function processNumKeyPairs(key:number, value:number):void {
    console.log('processNumKeyPairs: key = ' + key + ', value = ' + value)
}