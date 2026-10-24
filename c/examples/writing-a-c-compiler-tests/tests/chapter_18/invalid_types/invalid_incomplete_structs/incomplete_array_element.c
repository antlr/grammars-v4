struct s;

// can't specify an array type whose element type is incomplete
// even if the type is completed later
struct s arr[3];

struct s {
    int a;
    int b;
};

int main(void) {
    return 0;
}