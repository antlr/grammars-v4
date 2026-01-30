// Can't cast operand to union type even if it already has that type

union u1 {
    int a;
};


int main(void){
    union u1 var = {10};
    (union u1) var; // illegal - no casts to union type
    return 0;
}