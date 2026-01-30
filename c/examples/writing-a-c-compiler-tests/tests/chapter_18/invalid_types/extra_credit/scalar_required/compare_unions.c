// Can't compare objects of union type

union u { long l; };

int main(void){
    union u x = {1};
    x == x; // illegal
    return 0;
}