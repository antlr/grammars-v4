/* Test that we can declare a function with incomplete parameter and return
 * types, then call/define it after the type is completed
 * */

struct s;
struct s increment_struct(struct s param);

// complete the type
struct s {
    int a;
    int b;
};

int main(void) {
    struct s arg = {1, 2};

    // we can call increment_struct now that the 'struct s' type is complete
    struct s val = increment_struct(arg);
    if (val.a != 2 || val.b != 3) {
        return 1;
    }
    return 0;  // success
}

struct s increment_struct(struct s param) {
    // increment both members
    param.a = param.a + 1;
    param.b = param.b + 1;
    return param;
}