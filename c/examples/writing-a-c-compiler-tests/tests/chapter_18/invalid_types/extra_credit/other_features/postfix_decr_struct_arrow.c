// Can't apply ++/-- to any structures, including nested ones accessed thru ->
struct inner {int i;};
struct outer{struct inner s;};

int main(void) {
    struct outer my_struct = {{1}};
    struct outer *ptr = &my_struct;
    ptr->s--;
    return 0;
}