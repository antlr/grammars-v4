/* You can't initialize a union with a scalar value */
union u {int a;};

int main(void){
    static union u my_union = 1;
    return 0;
}