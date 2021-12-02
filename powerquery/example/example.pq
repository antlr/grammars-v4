section testSection;

myrecord = [
    func = (arg, optional arg1 as text) => { "foo", 1, false, 7.5}
];

CustomType = type table [
    Ttype = text,
    Ntype = number,
    LType = logical,
    Btype = binary,
    Dtype = datetime
];

// comment
tryCatcher = (entity as text) as type =>
    try 
        SchemaTable{[Entity=entity]}[Type]
    otherwise
        let
            testLet = true,
            nextLine = "7"
        in
            nextLine;
