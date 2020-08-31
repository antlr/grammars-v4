struct Aaa {
    [[attr1 ]] int aa; // Ok
    [[attr2(1)]] int bb; // not Ok, 
};