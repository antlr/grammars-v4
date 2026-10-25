// can't assign one struct type to another

struct s1 {
    int field;
};

struct s2 {
    int field;
};

int main(void) {
    struct s1 a = {1}   ;
    struct s2 b;
    b = a; // can't assign to struct s2 from struct s1
    return b.field;
}