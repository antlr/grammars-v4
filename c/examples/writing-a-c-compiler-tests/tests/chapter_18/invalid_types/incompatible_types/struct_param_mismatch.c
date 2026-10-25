struct one {
  int x;
  int y;
};

struct two {
  int a;
  int b;
};

int take_struct_param(struct one param) {
    return param.x;
}

int main(void) {
    struct two arg = {1, 2};
    return take_struct_param(arg); // can't convert argument of type "struct two" to parameter of type "struct one"
}