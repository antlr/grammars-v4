// Declare a structure inside a switch statement (basically just to make sure
// we're resolving structure tags inside switch statements)
struct s {
    int a;
    int b;
};

int main(void) {
    struct s my_struct = {1, 2};
    int result = 0;
    switch (my_struct.a) {
        // even though switch statement jumps over this declaration,
        // it's still in scope, shadowing outer one
        struct s {
            double x;
            double y;
            double z;
        };
        // declare inner variable, shadowing outer one
        struct s my_struct;
        case 1:
            my_struct.x = 20.0;
            my_struct.y = 30.0;
            result = my_struct.x + my_struct.y;
            break;
        case 2:
            my_struct.x = 11.;
            my_struct.y = 12.;
            result = my_struct.x + my_struct.y;
            break;
        default:
            my_struct.x = 0.;
            my_struct.y = 0.;
            result = my_struct.x + my_struct.y;
    }
    return result; // expected result is 50
}