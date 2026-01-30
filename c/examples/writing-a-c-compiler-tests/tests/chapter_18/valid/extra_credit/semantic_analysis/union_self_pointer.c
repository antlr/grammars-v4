// A union type can't have itself as a member but can have a pointer
// to itself as a member
union self_ptr {
    union self_ptr *ptr;
    long l;
};

int main(void) {
    union self_ptr u = {&u};
    if (&u != u.ptr) {
        return 1; // fail
    }
    return 0;
}