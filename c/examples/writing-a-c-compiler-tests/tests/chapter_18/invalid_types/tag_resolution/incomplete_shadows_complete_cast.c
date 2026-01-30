// test that we resolve tags in cast expressions
void *malloc(unsigned long size);
struct s {
    int a;
};

int main(void) {
    void *ptr = malloc(sizeof(struct s));
    struct s;  // declare a new, incomplete type 'struct s'
    // this cast is illegal, because the 'struct s' specifier refers to inner,
    // incomplete type, which does not have a member 'a'
    ((struct s *)ptr)->a = 10;
    return 0;
}