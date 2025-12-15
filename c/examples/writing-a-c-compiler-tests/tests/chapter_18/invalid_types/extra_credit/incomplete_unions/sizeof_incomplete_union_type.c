// Can't apply sizeof to an incomplete union type

int main(void) {
    union u;
    return sizeof(union u);  // invalid - union u type is incomplete
}