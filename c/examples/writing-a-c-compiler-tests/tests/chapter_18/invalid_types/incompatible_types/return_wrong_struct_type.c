struct one {
  int x;
  int y;
};

struct two {
  int a;
  int b;
};

struct one return_struct(void) {
    struct two retval = {1, 2};
    return retval; // can't return a "struct two" from function w/ return type "struct one"
}

int main(void) {
    return return_struct().x;
}