/* CopyToOffset does not kill src struct */

struct s {
    int a;
    int b;
    int c;
};

struct s glob = {1, 2, 3};

int main(void) {
    struct s my_struct = glob;  // not a dead store
    my_struct.c = 100;          // this doesn't make my_struct dead
    if (my_struct.c != 100 ) {
        return 1; // fail
    }
    if (my_struct.a != 1) {
        return 2; // fail
    }
    if (glob.c != 3) {
        return 3; // fail
    }
    return 0; // success
}