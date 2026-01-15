int main(void) {
    struct x;
    x y;  // you can't specify a structure type with just the tag; you need the
          // 'struct' specifier
    return 0;
}