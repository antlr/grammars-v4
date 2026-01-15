/* compound assignment to struct members can be a dead store */

struct s {
    int i;
};

struct s glob_struct = { 15 };
int target(void) {
    struct s my_struct = { 4 }; // dead (because compound assign below is dead too)
    my_struct.i /= 2; // dead!
    my_struct = glob_struct;
    return my_struct.i;
}

int main(void) {
    return target();
}