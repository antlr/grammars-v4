let employeeName = "John";
// or
let employeeName:string = "John";

var num1:number = 1;

const playerCodes = {
    player1 : 9,
    player2 : 10,
    player3 : 13,
    player4 : 20
};
playerCodes.player2 = 11; // OK

playerCodes = {     //Compiler Error: Cannot assign to playerCodes because it is a constant or read-only
    player1 : 50,   // Modified value
    player2 : 10,
    player3 : 13,
    player4 : 20
};

playerCodesArray = {     //Compiler Error: Cannot assign to playerCodes because it is a constant or read-only
    player1 : 50,   // Modified value
    player2 : playerCodes[Test],
    player3 : 13,
    player4 : 20
};