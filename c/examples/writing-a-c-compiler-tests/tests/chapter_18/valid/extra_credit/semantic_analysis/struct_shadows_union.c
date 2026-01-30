// One type declaration can shadow another with the same tag
void *malloc(unsigned long size);

int main(void) {
    struct s {int a; int b;};
    struct s my_struct = {12, 13};
    {
        // union type declaration shadows declaration of struct s
        union u;
        union u *ptr = malloc(4);
        union u {int i; unsigned int u;};
        ptr->i = 10;
        if (ptr->u != 10) {
            return 1; // fail
        }
        if (my_struct.b != 13) {
            return 2; // fail
        }
    }

    return 0; // success
}