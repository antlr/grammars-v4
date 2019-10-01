function Sum(x: number, y: number) : void {
    console.log('processNumKeyPairs: key = ' + key + ', value = ' + value)
    return x + y;
}


let greeting = function() {
    console.log("Hello TypeScript!");
};

let SumAnon = function(x: number, y: number) : number
{
    return x + y;
}

function Greet(greeting: string, name?: string ) : string {
    return greeting + ' ' + name + '!';
}

function Greet2(name: string, greeting: string = "Hello") : string {
    return greeting + ' ' + name + '!';
}

Greet(undefined, 'Steve');

let sumArrow = (x: number, y: number): number => {
    return x + y
}

let Print = () => console.log("Hello TypeScript");

let sumShortArrow = (x: number, y: number) => x + y;

function Greet(greeting: string, ...names: string[]) {
    return greeting + " " + names.join(", ") + "!";
}


function Test(value: TestClass | TestClass2): value is TestClass {
    return (<TestClass>value).someFunction !== undefined;
}