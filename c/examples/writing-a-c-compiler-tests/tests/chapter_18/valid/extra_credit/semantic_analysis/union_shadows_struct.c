// A union type declaration can shadow a struct type declaration with the same tag
struct tag {
    int a;
    int b;
};

struct tag global_struct = {1, 2};

int main(void) {
    // this shadows the declaration of 'struct tag'
    union tag {
        int x;
        long y;
    };
    union tag local_union = {100};
    if (global_struct.a != 1) {
        return 1;  // fail
    }
    if (local_union.x != 100) {
        return 2;  // fail
    }
    return 0;  // success
}