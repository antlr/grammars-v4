function getArray<T>(items : T[] ) : T[] {
    return new Array<T>().concat();
}

let myNumArr = getArray<Test>([100, 200, 300]);
let myStrArr = getArray<string>(["Hello", "World"]);

myNumArr.push(400); // OK
myStrArr.push("Hello TypeScript"); // OK

myNumArr.push("Hi"); // Compiler Error
myStrArr.push(500); // Compiler Error


function displayType<T, U>(id:T, name:U): void {
    console.log(typeof(id) + ", " + typeof(name));
}

function displayTypeNon<T>(id:T, name:string): void {
    console.log(typeof(id) + ", " + typeof(name));
}

function displayNames<T>(names:T[]): void {
    console.log(names.join(", "));
}


function display<T extends Person>(per: T): void {
    console.log(`${ per.firstName} ${per.lastName}` );
}