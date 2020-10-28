let x: number = 10, y = 20;

// If-else

if (x > y)
{
    console.log('x is greater than y.');
}
else
{
    console.log('x is less than or equal to y.'); //This will be executed
}

// Switch
switch (x-y) {
    case 0:
        console.log("Result: 0");
        break;
    case 5:
        console.log("Result: 5");
        break;
    case 10:
        console.log("Result: 10");
        break;
}

// For
for (let i = 0; i < 3; i++) {
    console.log ("Block statement execution no." + i);
}

let str = "Hello World";

for (var char of str) {
    console.log(char); // prints chars: H e l l o  W o r l d
}

let arr = [10, 20, 30, 40];

for (var index in arr) {
    console.log(index); // prints indexes: 0, 1, 2, 3

    console.log(arr[index]); // prints elements: 10, 20, 30, 40
}

// While
let i: number = 2;
do {
    console.log("Block statement execution no." + i )
    i++;
} while ( i < 4)

while (i < 4) {
    console.log( "Block statement execution no." + i )
    i++;
}