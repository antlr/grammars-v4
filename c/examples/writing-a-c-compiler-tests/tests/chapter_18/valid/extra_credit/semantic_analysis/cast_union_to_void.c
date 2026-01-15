// You can cast a union to void

union u {
    long l;
    double d;
};

int main(void) {
    union u x = {1000};
    (void) x; // just make sure this doesn't cause a type error
    return 0;
}