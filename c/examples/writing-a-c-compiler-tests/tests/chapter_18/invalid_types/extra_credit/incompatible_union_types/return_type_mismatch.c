union u {
    int x;
};

union u return_union(void){
    // define an identical but distinct union type
    union u {
        int x;
    };

    union u result = {10};

    // return it; invalid b/c it's the wrong "union u" type
    return result;
}
