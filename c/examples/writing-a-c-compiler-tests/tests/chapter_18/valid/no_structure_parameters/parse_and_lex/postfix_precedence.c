// postfix operators have higher precedence than prefix
struct inner {
    int inner_arr[3];
};

struct outer {
    int a;
    struct inner b;
};

int main(void) {
    struct outer array[4] = {{1, {{2, 3, 4}}},
                             {5, {{6, 7, 8}}},
                             {9, {{10, 11, 12}}},
                             {13, {{14, 15, 16}}}};

    int i = -array[2].b.inner_arr[1];
    return i == -11;
}