// Can't assign to members in non-lvalue unions
union inner {
    int y;
    long z;
};

union u {
    int x;
    union inner i;
};

union u return_union(void){
    union u result = {1};
    return result;
}

int main(void) {
    // invalid - return_union() is not an lvalue
    return_union().i.y = 1;
    return 0;
}