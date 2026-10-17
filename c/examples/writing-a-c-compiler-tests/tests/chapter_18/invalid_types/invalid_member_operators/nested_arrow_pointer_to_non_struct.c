struct s {
    long l;
};

struct has_ptr {
    double *ptr;
};

int main(void) {
    double d = 0.0;
    struct has_ptr p_struct = { &d };
    return p_struct.ptr->l;  // can't apply -> operator to pointer to non-struct
}