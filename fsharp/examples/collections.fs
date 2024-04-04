let people = seq { "Tom"; "Sam"; "Bob"; "Alice"; "Mike"}

let squares = [ for i in 1..5 -> i * i ] // [1; 4; 9; 16; 25]

let people2 = ["Tom"; "Bob"; "Alice"; "Mike"; "Sam"]

let people3 = [|
    "Tom"
    "Sam"
    "Bob"
|]

let dict = Map[
    "red", "красный"
    "blue", "синий"
    "green", "зеленый"
]

let people5 = set ["Tom"; "Bob"; "Sam"]